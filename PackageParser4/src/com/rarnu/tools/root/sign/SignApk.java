package com.rarnu.tools.root.sign;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.DEROutputStream;
import org.bouncycastle.cert.jcajce.JcaCertStore;
import org.bouncycastle.cms.*;
import org.bouncycastle.cms.jcajce.JcaSignerInfoGeneratorBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.OperatorCreationException;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import org.bouncycastle.operator.jcajce.JcaDigestCalculatorProviderBuilder;
import org.bouncycastle.util.encoders.Base64;

import java.io.*;
import java.security.*;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.*;
import java.util.jar.*;
import java.util.regex.Pattern;

public class SignApk {

    private static final String CERT_SF_NAME = "META-INF/CERT.SF";
    private static final String CERT_RSA_NAME = "META-INF/CERT.RSA";
    private static final String OTACERT_NAME = "META-INF/com/android/otacert";
    private static Provider sBouncyCastleProvider;
    private static Pattern stripPattern = Pattern.compile("^(META-INF/((.*)[.](SF|RSA|DSA)|com/android/otacert))|(" + Pattern.quote(JarFile.MANIFEST_NAME) + ")$");

    private static X509Certificate readPublicKey(File file) throws IOException, GeneralSecurityException {
        FileInputStream input = new FileInputStream(file);
        try {
            CertificateFactory cf = CertificateFactory.getInstance("X.509");
            return (X509Certificate) cf.generateCertificate(input);
        } finally {
            input.close();
        }
    }

    private static PrivateKey readPrivateKey(File file) throws IOException, GeneralSecurityException {
        DataInputStream input = new DataInputStream(new FileInputStream(file));
        try {
            byte[] bytes = new byte[(int) file.length()];
            input.read(bytes);

            KeySpec spec = new PKCS8EncodedKeySpec(bytes);

            try {
                return KeyFactory.getInstance("RSA").generatePrivate(spec);
            } catch (InvalidKeySpecException ex) {
                return KeyFactory.getInstance("DSA").generatePrivate(spec);
            }
        } finally {
            input.close();
        }
    }

    public static Manifest addDigestsToManifest(JarFile jar) throws IOException, GeneralSecurityException {
        Manifest input = jar.getManifest();
        Manifest output = new Manifest();
        Attributes main = output.getMainAttributes();
        if (input != null) {
            main.putAll(input.getMainAttributes());
        } else {
            main.putValue("Manifest-Version", "1.0");
            main.putValue("Created-By", "1.0 (Android SignApk)");
        }

        MessageDigest md = MessageDigest.getInstance("SHA1");
        byte[] buffer = new byte[4096];
        int num;

        TreeMap<String, JarEntry> byName = new TreeMap<String, JarEntry>();

        for (Enumeration<JarEntry> e = jar.entries(); e.hasMoreElements(); ) {
            JarEntry entry = e.nextElement();
            byName.put(entry.getName(), entry);
        }

        for (JarEntry entry : byName.values()) {
            String name = entry.getName();
            if (!entry.isDirectory() && (stripPattern == null || !stripPattern.matcher(name).matches())) {
                InputStream data = jar.getInputStream(entry);
                while ((num = data.read(buffer)) > 0) {
                    md.update(buffer, 0, num);
                }

                Attributes attr = null;
                if (input != null) attr = input.getAttributes(name);
                attr = attr != null ? new Attributes(attr) : new Attributes();
                attr.putValue("SHA1-Digest", new String(Base64.encode(md.digest()), "ASCII"));
                output.getEntries().put(name, attr);
            }
        }

        return output;
    }

    public static void addOtacert(JarOutputStream outputJar, File publicKeyFile, long timestamp, Manifest manifest) throws IOException, GeneralSecurityException {
        MessageDigest md = MessageDigest.getInstance("SHA1");

        JarEntry je = new JarEntry(OTACERT_NAME);
        je.setTime(timestamp);
        outputJar.putNextEntry(je);
        FileInputStream input = new FileInputStream(publicKeyFile);
        byte[] b = new byte[4096];
        int read;
        while ((read = input.read(b)) != -1) {
            outputJar.write(b, 0, read);
            md.update(b, 0, read);
        }
        input.close();

        Attributes attr = new Attributes();
        attr.putValue("SHA1-Digest", new String(Base64.encode(md.digest()), "ASCII"));
        manifest.getEntries().put(OTACERT_NAME, attr);
    }

    private static void writeSignatureFile(Manifest manifest, OutputStream out) throws IOException, GeneralSecurityException {
        Manifest sf = new Manifest();
        Attributes main = sf.getMainAttributes();
        main.putValue("Signature-Version", "1.0");
        main.putValue("Created-By", "1.0 (Android SignApk)");

        MessageDigest md = MessageDigest.getInstance("SHA1");
        PrintStream print = new PrintStream(new DigestOutputStream(new ByteArrayOutputStream(), md), true, "UTF-8");

        manifest.write(print);
        print.flush();
        main.putValue("SHA1-Digest-Manifest", new String(Base64.encode(md.digest()), "ASCII"));

        Map<String, Attributes> entries = manifest.getEntries();
        for (Map.Entry<String, Attributes> entry : entries.entrySet()) {
            print.print("Name: " + entry.getKey() + "\r\n");
            for (Map.Entry<Object, Object> att : entry.getValue().entrySet()) {
                print.print(att.getKey() + ": " + att.getValue() + "\r\n");
            }
            print.print("\r\n");
            print.flush();

            Attributes sfAttr = new Attributes();
            sfAttr.putValue("SHA1-Digest", new String(Base64.encode(md.digest()), "ASCII"));
            sf.getEntries().put(entry.getKey(), sfAttr);
        }

        CountOutputStream cout = new CountOutputStream(out);
        sf.write(cout);
        if ((cout.size() % 1024) == 0) {
            cout.write('\r');
            cout.write('\n');
        }
    }

    public static void writeSignatureBlock(CMSTypedData data, X509Certificate publicKey, PrivateKey privateKey, OutputStream out) throws IOException, CertificateEncodingException, OperatorCreationException, CMSException {
        ArrayList<X509Certificate> certList = new ArrayList<X509Certificate>(1);
        certList.add(publicKey);
        JcaCertStore certs = new JcaCertStore(certList);

        CMSSignedDataGenerator gen = new CMSSignedDataGenerator();
        ContentSigner sha1Signer = new JcaContentSignerBuilder("SHA1withRSA")
                .setProvider(sBouncyCastleProvider)
                .build(privateKey);
        gen.addSignerInfoGenerator(
                new JcaSignerInfoGeneratorBuilder(
                        new JcaDigestCalculatorProviderBuilder()
                                .setProvider(sBouncyCastleProvider)
                                .build())
                        .setDirectSignature(true)
                        .build(sha1Signer, publicKey));
        gen.addCertificates(certs);
        CMSSignedData sigData = gen.generate(data, false);

        ASN1InputStream asn1 = new ASN1InputStream(sigData.getEncoded());
        DEROutputStream dos = new DEROutputStream(out);
        dos.writeObject(asn1.readObject());
    }

    private static void copyFiles(Manifest manifest, JarFile in, JarOutputStream out, long timestamp) throws IOException {
        byte[] buffer = new byte[4096];
        int num;

        Map<String, Attributes> entries = manifest.getEntries();
        ArrayList<String> names = new ArrayList<String>(entries.keySet());
        Collections.sort(names);
        for (String name : names) {
            JarEntry inEntry = in.getJarEntry(name);
            JarEntry outEntry;
            if (inEntry.getMethod() == JarEntry.STORED) {
                outEntry = new JarEntry(inEntry);
            } else {
                outEntry = new JarEntry(name);
            }
            outEntry.setTime(timestamp);
            out.putNextEntry(outEntry);

            InputStream data = in.getInputStream(inEntry);
            while ((num = data.read(buffer)) > 0) {
                out.write(buffer, 0, num);
            }
            out.flush();
        }
    }

    private static void signWholeFile(JarFile inputJar, File publicKeyFile, X509Certificate publicKey, PrivateKey privateKey, OutputStream outputStream) throws Exception {
        CMSSigner cmsOut = new CMSSigner(inputJar, publicKeyFile, publicKey, privateKey, outputStream);

        ByteArrayOutputStream temp = new ByteArrayOutputStream();

        byte[] message = "signed by SignApk".getBytes("UTF-8");
        temp.write(message);
        temp.write(0);

        cmsOut.writeSignatureBlock(temp);

        byte[] zipData = cmsOut.getSigner().getTail();

        if (zipData[zipData.length - 22] != 0x50 || zipData[zipData.length - 21] != 0x4b || zipData[zipData.length - 20] != 0x05 || zipData[zipData.length - 19] != 0x06) {
            throw new IllegalArgumentException("zip data already has an archive comment");
        }

        int total_size = temp.size() + 6;
        if (total_size > 0xffff) {
            throw new IllegalArgumentException("signature is too big for ZIP file comment");
        }

        int signature_start = total_size - message.length - 1;
        temp.write(signature_start & 0xff);
        temp.write((signature_start >> 8) & 0xff);
        temp.write(0xff);
        temp.write(0xff);
        temp.write(total_size & 0xff);
        temp.write((total_size >> 8) & 0xff);
        temp.flush();

        byte[] b = temp.toByteArray();
        for (int i = 0; i < b.length - 3; ++i) {
            if (b[i] == 0x50 && b[i + 1] == 0x4b && b[i + 2] == 0x05 && b[i + 3] == 0x06) {
                throw new IllegalArgumentException("found spurious EOCD header at " + i);
            }
        }

        outputStream.write(total_size & 0xff);
        outputStream.write((total_size >> 8) & 0xff);
        temp.writeTo(outputStream);
    }

    public static void signFile(Manifest manifest, JarFile inputJar, X509Certificate publicKey, PrivateKey privateKey, JarOutputStream outputJar) throws Exception {
        long timestamp = publicKey.getNotBefore().getTime() + 3600L * 1000;

        JarEntry je;
        copyFiles(manifest, inputJar, outputJar, timestamp);

        je = new JarEntry(JarFile.MANIFEST_NAME);
        je.setTime(timestamp);
        outputJar.putNextEntry(je);
        manifest.write(outputJar);

        je = new JarEntry(CERT_SF_NAME);
        je.setTime(timestamp);
        outputJar.putNextEntry(je);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        writeSignatureFile(manifest, baos);
        byte[] signedData = baos.toByteArray();
        outputJar.write(signedData);
        je = new JarEntry(CERT_RSA_NAME);
        je.setTime(timestamp);
        outputJar.putNextEntry(je);
        writeSignatureBlock(new CMSProcessableByteArray(signedData), publicKey, privateKey, outputJar);

    }

    public static void sign(String publicKeyFile, String privateKeyFile, String inputApk, String outputApk, boolean signWholeFile) {

        sBouncyCastleProvider = new BouncyCastleProvider();
        Security.addProvider(sBouncyCastleProvider);

        String inputFilename = inputApk;
        String outputFilename = outputApk;

        JarFile inputJar = null;
        FileOutputStream outputFile = null;

        try {
            File firstPublicKeyFile = new File(publicKeyFile);
            X509Certificate publicKey = readPublicKey(new File(publicKeyFile));
            PrivateKey privateKey = readPrivateKey(new File(privateKeyFile));
            inputJar = new JarFile(new File(inputFilename), false);
            outputFile = new FileOutputStream(outputFilename);


            if (signWholeFile) {
                SignApk.signWholeFile(inputJar, firstPublicKeyFile, publicKey, privateKey, outputFile);
            } else {
                JarOutputStream outputJar = new JarOutputStream(outputFile);
                outputJar.setLevel(9);

                signFile(addDigestsToManifest(inputJar), inputJar, publicKey, privateKey, outputJar);
                outputJar.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (inputJar != null) inputJar.close();
                if (outputFile != null) outputFile.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}