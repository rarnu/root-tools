package com.snda.gyue.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Environment;
import android.util.Log;

/**
 * MiscUtils<br>
 * Download, Network state, ...
 * 
 * @author rarnu
 * 
 */
public class MiscUtils {
	
	public static boolean sdcardExists() {
		return (Environment.getExternalStorageState().equals(Environment.MEDIA_MOUNTED));
	}

	public static String getFirmwareVersion(Context context) {
		String firmwareVerPath = "/system/app/firmware.ver";
		String ret = "";
		XmlUtils xml = new XmlUtils();
		xml.initialize();
		if (xml.loadFile(context, new File(firmwareVerPath))) {
			NodeList nodes = xml.getRoot().getElementsByTagName("firmware").item(0).getChildNodes();
			for (int i = 0; i < nodes.getLength(); i++) {
				if (nodes.item(i) instanceof Element) {
					if (nodes.item(i).getNodeName().equals("manualversion")) {
						ret = xml.getNodeValue((Element) nodes.item(i));
						break;
					}

				}
			}
		}
		xml.finalize();
		return ret;
	}

	/**
	 * check data's encoding<br>
	 * returns true for encoding is utf8 and false for not.
	 * 
	 * @param data
	 * @return
	 */
	public boolean isUTF8(byte[] data) {
		int charByteCounter = 1;
		byte curByte;
		for (int i = 0; i < data.length; i++) {
			curByte = data[i];
			if (charByteCounter == 1) {
				if (curByte >= 0x80) {
					while (((curByte <<= 1) & 0x80) != 0) {
						charByteCounter++;
					}
					if (charByteCounter == 1 || charByteCounter > 6) {
						return false;
					}
				}
			} else {
				if ((curByte & 0xC0) != 0x80) {
					return false;
				}
				charByteCounter--;
			}
		}
		if (charByteCounter > 1) {
			return false;
		}
		return true;
	}

	/**
	 * Download a file from network
	 * 
	 * @param address
	 * @param savePath
	 */
	public static void downloadFile(String address, String savePath) {
		URL url = null;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			File fileOut = new File(savePath);
			FileOutputStream out = new FileOutputStream(fileOut);
			byte[] bytes = new byte[1024];
			int c;
			while ((c = in.read(bytes)) != -1) {
				out.write(bytes, 0, c);
			}
			in.close();
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * get network type<br>
	 * <br>
	 * returns:<br>
	 * 0: no connection<br>
	 * 1: wifi<br>
	 * 2: 3G<br>
	 * 3: 2G<br>
	 * 4: other<br>
	 * 
	 * @param inContext
	 * @return
	 * 
	 */
	public static int getNetworkType(Context inContext) {
		Context context = inContext.getApplicationContext();
		ConnectivityManager connectivity = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
		int ret = 4;
		if (connectivity != null) {
			NetworkInfo[] info = connectivity.getAllNetworkInfo();
			if (info != null) {
				for (int i = 0; i < info.length; i++) {

					String typ = info[i].getTypeName().toUpperCase();
					String subTyp = info[i].getSubtypeName().toUpperCase();
					Log.v("connection", typ + ":" + subTyp);
					if (typ.equals("WIFI") && info[i].isConnected()) {
						if (ret > 1) {
							ret = 1;
						}
					} else if (typ.equals("MOBILE") && (subTyp.equals("UMTS") || subTyp.equals("HSDPA"))
							&& info[i].isConnected()) {
						if (ret > 2) {
							ret = 2;
						}
					} else if (typ.equals("MOBILE") && (subTyp.equals("EDGE") || subTyp.equals("GPRS"))
							&& info[i].isConnected()) {
						if (ret > 3) {
							ret = 3;
						}
					}
				}
			}
		}
		return ret;
	}

	/**
	 * Get the File's UTC
	 * 
	 * @param context
	 * @param fileName
	 * @return
	 */
	public static long getFileUTC(Context context, String fileName) {
		try {
			File f = context.getFileStreamPath(fileName);
			return f.lastModified();
		} catch (Exception e) {
			return 0;
		}
	}
}
