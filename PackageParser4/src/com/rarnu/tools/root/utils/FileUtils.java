package com.rarnu.tools.root.utils;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * @hide
 */
class FileUtils {

    public static boolean mkdir(String path) {
        boolean ret = false;
        File myDir = new File(path);
        if (!myDir.exists()) {
            ret = myDir.mkdirs();
        }
        return ret;
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
