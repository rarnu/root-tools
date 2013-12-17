package com.rarnu.tools.root.sign;

import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.cms.CMSException;
import org.bouncycastle.cms.CMSTypedData;
import org.bouncycastle.operator.OperatorCreationException;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.security.PrivateKey;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;

class CMSSigner implements CMSTypedData {
    private final ASN1ObjectIdentifier type;
    private JarFile inputJar;
    private File publicKeyFile;
    private X509Certificate publicKey;
    private PrivateKey privateKey;
    private OutputStream outputStream;
    private WholeFileSignerOutputStream signer;

    public CMSSigner(JarFile inputJar, File publicKeyFile, X509Certificate publicKey, PrivateKey privateKey, OutputStream outputStream) {
        this.inputJar = inputJar;
        this.publicKeyFile = publicKeyFile;
        this.publicKey = publicKey;
        this.privateKey = privateKey;
        this.outputStream = outputStream;
        this.type = new ASN1ObjectIdentifier(CMSObjectIdentifiers.data.getId());
    }

    public Object getContent() {
        throw new UnsupportedOperationException();
    }

    public ASN1ObjectIdentifier getContentType() {
        return type;
    }

    public void write(OutputStream out) throws IOException {
        try {
            signer = new WholeFileSignerOutputStream(out, outputStream);
            JarOutputStream outputJar = new JarOutputStream(signer);

            Manifest manifest = SignApk.addDigestsToManifest(inputJar);
            SignApk.signFile(manifest, inputJar, publicKey, privateKey, outputJar);
            long timestamp = publicKey.getNotBefore().getTime() + 3600L * 1000;
            SignApk.addOtacert(outputJar, publicKeyFile, timestamp, manifest);

            signer.notifyClosing();
            outputJar.close();
            signer.finish();
        } catch (Exception e) {
            throw new IOException(e);
        }
    }

    public void writeSignatureBlock(ByteArrayOutputStream temp) throws IOException, CertificateEncodingException, OperatorCreationException, CMSException {
        SignApk.writeSignatureBlock(this, publicKey, privateKey, temp);
    }

    public WholeFileSignerOutputStream getSigner() {
        return signer;
    }
}