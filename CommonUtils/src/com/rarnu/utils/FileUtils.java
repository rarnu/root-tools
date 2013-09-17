package com.rarnu.utils;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.os.Handler;
import android.os.Message;

public class FileUtils {

	public static final int WHAT_COPY_START = 1;
	public static final int WHAT_COPY_PROGRESS = 2;
	public static final int WHAT_COPY_FINISH = 3;

	public static boolean mkdir(String path) {
		boolean ret = false;
		File myDir = new File(path);
		if (!myDir.exists()) {
			ret = myDir.mkdirs();
		}
		return ret;
	}

	public static void rewriteFile(File file, String text, String encoding)
			throws IOException {
		writeFileByStream(file, text, false, encoding);
	}

	public static void rewriteFile(File file, String text) throws IOException {
		writeFileByWriter(file, text, false);
	}

	public static void rewriteFile(String path, String text) throws IOException {
		File myFile = new File(path);
		rewriteFile(myFile, text);
	}

	public static void rewriteFile(String path, String text, String encoding)
			throws IOException {
		File myFile = new File(path);
		rewriteFile(myFile, text, encoding);
	}

	public static void appendFile(File file, String text) throws IOException {
		writeFileByWriter(file, text, true);
	}

	public static void appendFile(File file, String text, String encoding)
			throws IOException {
		writeFileByStream(file, text, true, encoding);
	}

	private static void writeFileByWriter(File file, String text, boolean append)
			throws IOException {
		FileWriter myFileWriter = new FileWriter(file, append);
		if (append) {
			myFileWriter.append(text);
		} else {
			myFileWriter.write(text);
		}
		myFileWriter.close();
	}

	private static void writeFileByStream(File file, String text,
			boolean append, String encoding) throws IOException {
		FileOutputStream fos = new FileOutputStream(file, append);
		OutputStreamWriter myWriter = new OutputStreamWriter(fos, encoding);
		myWriter.write(text);
		myWriter.close();
		fos.close();
	}

	public static void appendFile(String path, String text) throws IOException {
		File myFile = new File(path);
		appendFile(myFile, text);
	}

	public static void appendFile(String path, String text, String encoding)
			throws IOException {
		File myFile = new File(path);
		appendFile(myFile, text, encoding);
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

	public static void copyFile(String source, String dest, Handler hProgress)
			throws IOException {

		File oldFile = new File(source);
		if (oldFile.exists()) {
			InputStream is = new FileInputStream(source);
			FileOutputStream fs = new FileOutputStream(dest);

			int size = is.available();
			if (hProgress != null) {
				Message msg = new Message();
				msg.what = WHAT_COPY_START;
				msg.arg1 = 0;
				msg.arg2 = size;
				hProgress.sendMessage(msg);
			}
			int count = 0;
			int n = 0;

			byte[] buffer = new byte[1444];
			while ((n = is.read(buffer)) != -1) {
				fs.write(buffer, 0, n);
				count += n;
				if (hProgress != null) {
					Message msg = new Message();
					msg.what = WHAT_COPY_PROGRESS;
					msg.arg1 = count;
					msg.arg2 = size;
					hProgress.sendMessage(msg);
				}
			}
			is.close();
			fs.close();
			if (hProgress != null) {
				Message msg = new Message();
				msg.what = WHAT_COPY_FINISH;
				msg.arg1 = 0;
				msg.arg2 = 0;
				hProgress.sendMessage(msg);
			}
		}
	}

	public static void copyFolder(String source, String dest)
			throws IOException {
		new File(dest).mkdirs();
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
				FileOutputStream output = new FileOutputStream(dest
						+ File.separator + (temp.getName()).toString());
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
				copyFolder(source + File.separator + file[i], dest
						+ File.separator + file[i]);
			}
		}

	}

	public static void moveFile(String source, String dest, Handler hProgress)
			throws IOException {
		copyFile(source, dest, hProgress);
		deleteFile(source);

	}

	public static void moveFolder(String source, String dest)
			throws IOException {
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

	public static String toString(List<String> list) {
		String ret = "";
		for (String s : list) {
			ret += s + "\n";
		}
		return ret;
	}

	public static List<String> readFile(String path) throws IOException {
		File myFile = new File(path);
		return readFile(myFile);
	}

	public static String readAssetFile(Context context, String fileName)
			throws IOException {
		InputStream is = context.getAssets().open(fileName);
		byte[] bytes = new byte[1024];
		ByteArrayOutputStream arrayOutputStream = new ByteArrayOutputStream();
		while (is.read(bytes) != -1) {
			arrayOutputStream.write(bytes, 0, bytes.length);
		}
		is.close();
		arrayOutputStream.close();
		String text = new String(arrayOutputStream.toByteArray());
		return text.trim();
	}

	public static List<String> readAssertFileAsList(Context context,
			String fileName) throws IOException {
		InputStream is = context.getAssets().open(fileName);
		InputStreamReader myStreamReader = new InputStreamReader(is);
		BufferedReader myBufferedReader = new BufferedReader(myStreamReader);
		String line;
		List<String> fileText = new ArrayList<String>();
		while ((line = myBufferedReader.readLine()) != null) {
			fileText.add(line);
		}
		myBufferedReader.close();
		myStreamReader.close();
		return fileText;
	}

	public static boolean copyAssetFile(Context context, String fileName,
			String saveDir, Handler hProgress) {
		File fAsset = new File(saveDir);
		if (!fAsset.exists()) {
			fAsset.mkdirs();
		}
		try {
			byte[] buffer = new byte[8192];

			File dest = new File(saveDir + fileName);

			if (dest.exists()) {
				dest.delete();
			}

			InputStream is = context.getAssets().open(fileName);
			OutputStream fos = new BufferedOutputStream(new FileOutputStream(
					dest));

			int count = 0;
			int size = is.available();
			if (hProgress != null) {
				Message msg = new Message();
				msg.what = WHAT_COPY_START;
				msg.arg1 = 0;
				msg.arg2 = size;
				hProgress.sendMessage(msg);
			}
			int n;
			while ((n = is.read(buffer, 0, buffer.length)) != -1) {
				fos.write(buffer, 0, n);
				count += n;
				if (hProgress != null) {
					Message msg = new Message();
					msg.what = WHAT_COPY_PROGRESS;
					msg.arg1 = count;
					msg.arg2 = size;
					hProgress.sendMessage(msg);
				}
			}

			is.close();
			fos.close();
			if (hProgress != null) {
				Message msg = new Message();
				msg.what = WHAT_COPY_FINISH;
				msg.arg1 = 0;
				msg.arg2 = 0;
				hProgress.sendMessage(msg);
			}
			return true;
		} catch (Exception ex) {
			return false;
		}
	}

	public static String readInnerFile(Context context, String path)
			throws IOException {
		InputStream is = context.openFileInput(path);
		return readFileStream(is);
	}

	public static String readFileString(String path) throws IOException {
		InputStream is = new FileInputStream(path);
		return readFileStream(is);
	}

	public static String readFileString(String path, String encoding)
			throws IOException {
		String ret = readFileString(path);
		return new String(ret.getBytes(), encoding);
	}

	private static String readFileStream(InputStream is) throws IOException {
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

	public static List<?> loadListFromFile(String path) {
		List<?> list = null;
		try {
			FileInputStream freader = new FileInputStream(path);
			ObjectInputStream objectInputStream = new ObjectInputStream(freader);
			list = (List<?>) objectInputStream.readObject();
			objectInputStream.close();
		} catch (Exception e) {

		}
		return list;
	}

	public static void saveListToFile(List<?> list, String path) {
		try {
			FileOutputStream outStream = new FileOutputStream(path);
			ObjectOutputStream objectOutputStream = new ObjectOutputStream(
					outStream);
			objectOutputStream.writeObject(list);
			outStream.close();
		} catch (Exception e) {

		}
	}

	public static long getDirSize(String path) {
		return getDirSize(new File(path));
	}

	public static long getDirSize(File dir) {
		if (dir == null) {
			return 0;
		}
		if (!dir.isDirectory()) {
			return 0;
		}
		long dirSize = 0;
		File[] files = dir.listFiles();
		for (File file : files) {
			if (file.isFile()) {
				dirSize += file.length();
			} else if (file.isDirectory()) {
				dirSize += file.length();
				dirSize += getDirSize(file);
			}
		}
		return dirSize;
	}

}
