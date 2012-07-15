package com.rarnu.tools.root.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class FileUtils {

    public static boolean mkdir(String path) {
        boolean ret = false;
        File myDir = new File(path);
        if (!myDir.exists()) {
            ret = myDir.mkdirs();
        }
        return ret;
    }

    public static void createFile(String path, String text) throws IOException {
        File myFile = new File(path);
        if (!myFile.exists()) {
            myFile.createNewFile();
        }
        if (!text.equals("")) {
            rewriteFile(myFile, text);
        }
    }

    public static void rewriteFile(File file, String text) throws IOException {
        FileWriter myFileWriter = new FileWriter(file);
        myFileWriter.write(text);
        myFileWriter.close();
    }

    public static void rewriteFile(String path, String text) throws IOException {
        File myFile = new File(path);
        rewriteFile(myFile, text);
    }

    public static void appendFile(File file, String text) throws IOException {
        FileWriter myFileWriter = new FileWriter(file);
        myFileWriter.append(text);
        myFileWriter.close();
    }

    public static void appendFile(String path, String text) throws IOException {
        File myFile = new File(path);
        appendFile(myFile, text);
    }

    public static boolean deleteFile(String path) {
        File myFile = new File(path);
        return myFile.delete();
    }

    public static boolean deleteDir(String path) {
        deleteSubFiles(path);
        File myDir = new File(path);
        return myDir.delete();
    }

    public static void deleteSubFiles(String path) {
        File myFile = new File(path);
        if (!myFile.exists()) { return; }
        if (!myFile.isDirectory()) { return; }
        String[] tempList = myFile.list();
        File temp = null;
        for (int i = 0; i < tempList.length; i++) {
            if (path.endsWith(File.separator)) {
                temp = new File(path + tempList[i]);
            } else {
                temp = new File(path + File.separator + tempList[i]);
            }
            if (temp.isFile()) {
                temp.delete();
            }
            if (temp.isDirectory()) {
                deleteSubFiles(path + File.separator + tempList[i]);
                deleteDir(path + File.separator + tempList[i]);
            }
        }
    }

    public static void copyFile(String source, String dest) throws IOException {
        int bytesum = 0;
        int byteread = 0;
        File oldFile = new File(source);
        if (oldFile.exists()) {
            InputStream inStream = new FileInputStream(source);
            FileOutputStream fs = new FileOutputStream(dest);
            byte[] buffer = new byte[1444];
            while ((byteread = inStream.read(buffer)) != -1) {
                bytesum += byteread;
                System.out.println(bytesum);
                fs.write(buffer, 0, byteread);
            }
            inStream.close();
        }
    }

    public static void copyFolder(String source, String dest) throws IOException {
        (new File(dest)).mkdirs();
        File a = new File(source);
        String[] file = a.list();
        File temp = null;
        for (int i = 0; i < file.length; i++) {
            if (source.endsWith(File.separator)) {
                temp = new File(source + file[i]);
            } else {
                temp = new File(source + File.separator + file[i]);
            }

            if (temp.isFile()) {
                FileInputStream input = new FileInputStream(temp);
                FileOutputStream output = new FileOutputStream(dest + File.separator + (temp.getName()).toString());
                byte[] b = new byte[1024 * 5];
                int len;
                while ((len = input.read(b)) != -1) {
                    output.write(b, 0, len);
                }
                output.flush();
                output.close();
                input.close();
            }
            if (temp.isDirectory()) {
                copyFolder(source + File.separator + file[i], dest + File.separator + file[i]);
            }
        }

    }

    public static void moveFile(String source, String dest) throws IOException {
        copyFile(source, dest);
        deleteFile(source);

    }

    public static void moveFolder(String source, String dest) throws IOException {
        copyFolder(source, dest);
        deleteDir(source);

    }

    public static List<String> readFile(File file) throws IOException {
        FileReader myFileReader = new FileReader(file);
        BufferedReader myBufferedReader = new BufferedReader(myFileReader);
        String line;
        List<String> fileText = new ArrayList<String>();
        while ((line = myBufferedReader.readLine()) != null) {
            fileText.add(line);
        }
        myBufferedReader.close();
        myFileReader.close();
        return fileText;
    }

    public static List<String> readFile(String path) throws IOException {
        File myFile = new File(path);
        return readFile(myFile);
    }

}
