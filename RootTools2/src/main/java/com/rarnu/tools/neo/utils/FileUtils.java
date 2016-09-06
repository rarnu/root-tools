package com.rarnu.tools.neo.utils;

import android.content.Context;

import java.io.*;
import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

public class FileUtils {

    public static int S_IRU = 400;
    public static int S_IRG = 40;
    public static int S_IRO = 4;
    public static int S_IWU = 200;
    public static int S_IWG = 20;
    public static int S_IWO = 2;
    public static int S_IXU = 100;
    public static int S_IXG = 10;
    public static int S_IXO = 1;
    public static int S_IRWU = S_IRU | S_IWU;
    public static int S_IRWG = S_IRG | S_IWG;
    public static int S_IRWO = S_IRO | S_IWO;
    public static int S_IRXU = S_IRU | S_IXU;
    public static int S_IRXG = S_IRG | S_IXG;
    public static int S_IRXO = S_IRO | S_IXO;
    public static int S_IRWXU = S_IRU | S_IWU | S_IXU;
    public static int S_IRWXG = S_IRG | S_IWG | S_IXG;
    public static int S_IRWXO = S_IRO | S_IWO | S_IXO;

    public static void rewriteFile(File file, String text) throws IOException {
        writeFileByWriter(file, text, false);
    }

    public static void rewriteFile(String path, String text) throws IOException {
        File f = new File(path);
        rewriteFile(f, text);
    }

    private static void writeFileByWriter(File file, String text, boolean append) throws IOException {
        FileWriter writer = new FileWriter(file, append);
        if (append) {
            writer.append(text);
        } else {
            writer.write(text);
        }
        writer.close();
    }

    public static List<String> readFile(String path) throws IOException {
        File f = new File(path);
        return readFile(f);
    }

    public static List<String> readFile(File file) throws IOException {
        FileReader reader = new FileReader(file);
        BufferedReader bufferReader = new BufferedReader(reader);
        String line;
        List<String> fileText = new ArrayList<>();
        while ((line = bufferReader.readLine()) != null) {
            fileText.add(line);
        }
        bufferReader.close();
        reader.close();
        return fileText;
    }

    public static boolean copyAssetFile(Context context, String fileName, String saveDir) {
        File fAsset = new File(saveDir);
        if (!fAsset.exists()) {
            fAsset.mkdirs();
        }
        try {
            byte[] buffer = new byte[1024 * 1024];
            File dest = new File(saveDir, fileName);
            if (dest.exists()) {
                dest.delete();
            }
            InputStream is = context.getAssets().open(fileName);
            OutputStream fos = new BufferedOutputStream(new FileOutputStream(dest));
            int n;
            while ((n = is.read(buffer, 0, buffer.length)) != -1) {
                fos.write(buffer, 0, n);
            }
            is.close();
            fos.close();
            return true;
        } catch (Exception ex) {
            return false;
        }
    }

    public static String readAssetFile(Context context, String fileName) throws IOException {
        InputStream is = context.getAssets().open(fileName);
        byte[] bytes = new byte[1024 * 1024];
        ByteArrayOutputStream arrayOutputStream = new ByteArrayOutputStream();
        while (is.read(bytes) != -1) {
            arrayOutputStream.write(bytes, 0, bytes.length);
        }
        is.close();
        arrayOutputStream.close();
        String text = new String(arrayOutputStream.toByteArray());
        return text.trim();
    }

    public static void setPermission(String filePath, int permission) {
        try {
            Class<?> cls = Class.forName("android.os.FileUtils");
            Method method = cls.getDeclaredMethod("setPermissions", String.class, int.class, int.class, int.class);
            method.setAccessible(true);
            method.invoke(null, filePath, permission, -1, -1);
        } catch(Exception e) {
        }
    }

    public static String getReadableFileSize(long size) {
        String[] units = new String[]{"K", "M", "G", "T", "P"};
        double nSize = size * 1L * 1.0f;
        double mod = 1024.0f;
        int i = 0;
        while (nSize >= mod) {
            nSize /= mod;
            i++;
        }
        DecimalFormat df = new DecimalFormat("#.##");
        return String.format("%s %s", df.format(nSize), units[i]);
    }

}
