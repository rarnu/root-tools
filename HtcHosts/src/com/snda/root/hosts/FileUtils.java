package com.snda.root.hosts;

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

/**
 * 文件操作基础工具类
 * 
 * @author rarnu
 */
public class FileUtils {

    /**
     * 新建一个目录
     * 
     * @param path
     * @@要创建的目录路径
     * @return 是否创建成功
     */
    public static boolean mkdir(String path) {
        boolean ret = false;
        File myDir = new File(path);
        if (!myDir.exists()) {
            ret = myDir.mkdirs();
        }
        return ret;
    }

    /**
     * 新建一个文件并写入文本内容
     * 
     * @param path
     *            文件路径
     * @param text
     *            文件内容
     * @throws IOException
     */
    public static void createFile(String path, String text) throws IOException {
        File myFile = new File(path);
        if (!myFile.exists()) {
            myFile.createNewFile();
        }
        if (!text.equals("")) {
            rewriteFile(myFile, text);
        }
    }

    /**
     * 重写一个文件，覆盖掉其原本的内容
     * 
     * @param file
     *            要重写的文件
     * @param text
     *            文件内容
     * @throws IOException
     */
    public static void rewriteFile(File file, String text) throws IOException {
        FileWriter myFileWriter = new FileWriter(file);
        myFileWriter.write(text);
        myFileWriter.close();
    }

    /**
     * 重写一个文件，覆盖掉其原本的内容
     * 
     * @param path
     *            要重写的文件路径
     * @param text
     *            文件内容
     * @throws IOException
     */
    public static void rewriteFile(String path, String text) throws IOException {
        File myFile = new File(path);
        rewriteFile(myFile, text);
    }

    /**
     * 追加文本到一个文件内
     * 
     * @param file
     *            要追加内容的文件
     * @param text
     *            要追加的内容
     * @throws IOException
     */
    public static void appendFile(File file, String text) throws IOException {
        FileWriter myFileWriter = new FileWriter(file);
        myFileWriter.append(text);
        myFileWriter.close();
    }

    /**
     * 追加文本到一个文件内
     * 
     * @param path
     *            要追加内容的文件路径
     * @param text
     *            要追加的内容
     * @throws IOException
     */
    public static void appendFile(String path, String text) throws IOException {
        File myFile = new File(path);
        appendFile(myFile, text);
    }

    /**
     * 删除文件
     * 
     * @param path
     *            文件路径
     * @return 是否删除成功
     */
    public static boolean deleteFile(String path) {
        File myFile = new File(path);
        return myFile.delete();
    }

    /**
     * 删除整个目录
     * 
     * @param path
     *            要删除的目录路径
     * @return 是否删除成功
     */
    public static boolean deleteDir(String path) {
        deleteSubFiles(path);
        File myDir = new File(path);
        return myDir.delete();
    }

    /**
     * 删除指定目录内所有文件，不包括目录本身
     * 
     * @param path
     *            路径
     */
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

    /**
     * 复制一个文件
     * 
     * @param source
     *            源路径
     * @param dest
     *            目标路径
     * @throws IOException
     */
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

    /**
     * 复制一个目录
     * 
     * @param source
     *            源路径
     * @param dest
     *            目标路径
     * @throws IOException
     */
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

    /**
     * 移动一个文件
     * 
     * @param source
     *            源路径
     * @param dest
     *            目标路径
     * @throws IOException
     */
    public static void moveFile(String source, String dest) throws IOException {
        copyFile(source, dest);
        deleteFile(source);

    }

    /**
     * 移动一个目录
     * 
     * @param source
     *            源路径
     * @param dest
     *            目标路径
     * @throws IOException
     */
    public static void moveFolder(String source, String dest) throws IOException {
        copyFolder(source, dest);
        deleteDir(source);

    }

    /**
     * 读取一个文件的内容
     * 
     * @param file
     *            要读取的文件
     * @return 文件内容
     * @throws IOException
     */
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

    /**
     * 读取一个文件的内容
     * 
     * @param path
     *            要读取的文件路径
     * @return 文件内容
     * @throws IOException
     */
    public static List<String> readFile(String path) throws IOException {
        File myFile = new File(path);
        return readFile(myFile);
    }

}
