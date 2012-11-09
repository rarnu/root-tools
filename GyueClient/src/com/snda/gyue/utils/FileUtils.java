package com.snda.gyue.utils;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;

/**
 * BBFileUtils<br>
 * for file operating
 * 
 * @author hexiaojie<br>
 *         Bambook in GuoKe
 */
public class FileUtils {

	/**
	 * Force make directories with any depth
	 * 
	 * @param path
	 * @return
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
	 * Create a file with text
	 * 
	 * @param path
	 * @param text
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
	 * Re-write an existing file
	 * 
	 * @param file
	 * @param text
	 * @throws IOException
	 */
	public static void rewriteFile(File file, String text) throws IOException {
		FileWriter myFileWriter = new FileWriter(file);
		myFileWriter.write(text);
		myFileWriter.close();
	}

	public static void rewriteFile(String path, List<String> text) throws IOException {
		File myFile = new File(path);
		rewriteFile(myFile, text);
	}

	/**
	 * Re-write an existing file
	 * 
	 * @param path
	 * @param text
	 * @throws IOException
	 */
	public static void rewriteFile(String path, String text) throws IOException {
		File myFile = new File(path);
		rewriteFile(myFile, text);
	}

	/**
	 * Re-write an existing file
	 * 
	 * @param file
	 * @param text
	 * @throws IOException
	 */
	public static void rewriteFile(File file, List<String> text) throws IOException {
		String str = "";
		if (text != null) {
			if (text.size() > 0) {
				for (String s : text) {
					str += s + "\n";
				}
			}
		}
		rewriteFile(file, str);
	}

	/**
	 * Re-write
	 * 
	 * @param context
	 * @param path
	 * @param text
	 * @throws IOException
	 */
	public static void rewriteFile(Context context, String path, String text) throws IOException {
		OutputStream os = context.openFileOutput(path, Context.MODE_PRIVATE);
		OutputStreamWriter osw = new OutputStreamWriter(os);
		osw.write(text);
		osw.close();
		os.close();
	}

	/**
	 * Append text after an existing file
	 * 
	 * @param file
	 * @param text
	 * @throws IOException
	 */
	public static void appendFile(File file, String text) throws IOException {
		FileWriter myFileWriter = new FileWriter(file);
		myFileWriter.append(text);
		myFileWriter.close();
	}

	/**
	 * Append text after an existing file
	 * 
	 * @param path
	 * @param text
	 * @throws IOException
	 */
	public static void appendFile(String path, String text) throws IOException {
		File myFile = new File(path);
		appendFile(myFile, text);
	}

	/**
	 * Append text after an existing file
	 * 
	 * @param file
	 * @param text
	 * @throws IOException
	 */
	public static void appendFile(File file, List<String> text) throws IOException {
		String str = "";
		if (text != null) {
			if (text.size() > 0) {
				for (String s : text) {
					str += s + "\n";
				}
			}
		}
		appendFile(file, str);
	}

	/**
	 * Delete an existing file
	 * 
	 * @param path
	 * @return
	 */
	public static boolean deleteFile(String path) {
		File myFile = new File(path);
		return myFile.delete();
	}

	/**
	 * Delete a diretory which contains files
	 * 
	 * @param path
	 * @return
	 */
	public static boolean deleteDir(String path) {
		deleteSubFiles(path);
		File myDir = new File(path);
		return myDir.delete();
	}

	/**
	 * Copy an existing file to another location
	 * 
	 * @param source
	 * @param dest
	 * @throws IOException
	 */
	public static void copyFile(String source, String dest) throws IOException {

		boolean copied = copyFile(new File(source), new File(dest));
		if (!copied) {
			throw new IOException();
		}
	}

	/**
	 * Copy an existing diretory to another location
	 * 
	 * @param source
	 * @param dest
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
				copyFile(temp.getAbsolutePath(), dest + File.separator + (temp.getName()).toString());

			}
			if (temp.isDirectory()) {
				copyFolder(source + File.separator + file[i], dest + File.separator + file[i]);
			}
		}

	}

	/**
	 * Move an existing file to another location
	 * 
	 * @param source
	 * @param dest
	 * @throws IOException
	 */
	public static void moveFile(String source, String dest) throws IOException {
		copyFile(source, dest);
		deleteFile(source);
	}

	/**
	 * Move an existing diretory to another location
	 * 
	 * @param source
	 * @param dest
	 * @throws IOException
	 */
	public static void moveFolder(String source, String dest) throws IOException {
		copyFolder(source, dest);
		deleteDir(source);
	}

	/**
	 * Read a file into a String List
	 * 
	 * @param file
	 * @return
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
	 * Read a file into a String List
	 * 
	 * @param path
	 * @return
	 * @throws IOException
	 */
	public static List<String> readFile(String path) throws IOException {
		File myFile = new File(path);
		return readFile(myFile);
	}

	public static String readAssetFile(Context context, String fileName) throws IOException {
		InputStream is = context.getAssets().open(fileName);
		byte[] bytes = new byte[1024];
		ByteArrayOutputStream arrayOutputStream = new ByteArrayOutputStream();
		while (is.read(bytes) != -1) {
			arrayOutputStream.write(bytes, 0, bytes.length);
		}
		is.close();
		arrayOutputStream.close();
		String text = new String(arrayOutputStream.toByteArray());
		return text;
	}

	/**
	 * Read a file into a String
	 * 
	 * @param context
	 * @param path
	 * @return
	 * @throws IOException
	 */
	public static String readFile(Context context, String path) throws IOException {
		InputStream is = context.openFileInput(path);
		byte[] bytes = new byte[1024];
		ByteArrayOutputStream arrayOutputStream = new ByteArrayOutputStream();
		while (is.read(bytes) != -1) {
			arrayOutputStream.write(bytes, 0, bytes.length);
		}
		is.close();
		arrayOutputStream.close();
		String text = new String(arrayOutputStream.toByteArray());
		return text;
	}

	/** @hide */
	private static void deleteSubFiles(String path) {
		File myFile = new File(path);
		if (!myFile.exists()) {
			return;
		}
		if (!myFile.isDirectory()) {
			return;
		}
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

	// copy a file from srcFile to destFile, return true if succeed, return
	// false if fail
	private static boolean copyFile(File srcFile, File destFile) {
		boolean result = false;
		try {
			InputStream in = new FileInputStream(srcFile);
			try {
				result = copyToFile(in, destFile);
			} finally {
				in.close();
			}
		} catch (IOException e) {
			result = false;
		}
		return result;
	}

	/**
	 * Copy data from a source stream to destFile. Return true if succeed,
	 * return false if failed.
	 */
	private static boolean copyToFile(InputStream inputStream, File destFile) {
		try {
			OutputStream out = new FileOutputStream(destFile);
			try {
				byte[] buffer = new byte[4096];
				int bytesRead;
				while ((bytesRead = inputStream.read(buffer)) >= 0) {
					out.write(buffer, 0, bytesRead);
				}
			} finally {
				out.close();
			}
			
			return true;
		} catch (IOException e) {
			return false;
		}
	}
	
	/**
	 * convert gbk file to utf-8 file
	 * 
	 * @param filename
	 * @throws Exception
	 */
	public static void GBKFileToUtf8File(String filename) throws Exception {
		final String shift = "GBK";
		final String utf8 = "utf-8";

		String backupfilename = filename + ".backup";
		File f = new File(filename);
		File outf = new File(backupfilename);
		f.renameTo(outf);

		java.io.FileInputStream in = new java.io.FileInputStream(backupfilename);
		java.io.InputStreamReader isr = new java.io.InputStreamReader(in, shift);
		java.io.BufferedReader br = (new java.io.BufferedReader(isr));

		// open output stream
		java.io.FileOutputStream out = new java.io.FileOutputStream(filename);
		java.io.BufferedWriter bw = new java.io.BufferedWriter(new java.io.OutputStreamWriter(out, utf8));

		char[] buffer = new char[4096];
		int len;
		while ((len = br.read(buffer)) != -1)
			bw.write(buffer, 0, len);
		br.close();
		bw.flush();
		bw.close();
	}

}
