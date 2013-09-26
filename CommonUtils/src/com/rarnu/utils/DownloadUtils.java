package com.rarnu.utils;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.os.Handler;
import android.os.Message;
import android.widget.ImageView;
import com.rarnu.utils.common.DownloadInfo;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public class DownloadUtils {

    public static final int WHAT_DOWNLOAD_START = 1;
    public static final int WHAT_DOWNLOAD_PROGRESS = 2;
    public static final int WHAT_DOWNLOAD_FINISH = 3;
    private static List<DownloadInfo> listDownloading = new ArrayList<DownloadInfo>();

    public static void stopDownloadTask(String localDir, final String localFile) {
        final String filePath = localDir + localFile;
        for (int i = 0; i < listDownloading.size(); i++) {
            if (listDownloading.get(i).fileName.equals(filePath)) {
                try {
                    listDownloading.get(i).thread.wait(5000);
                    listDownloading.get(i).thread.interrupt();
                } catch (Exception e) {

                }
                listDownloading.remove(i);
                break;
            }
        }
    }

    public static void downloadFileT(final Context context, final ImageView iv, final String url, String localDir, final String localFile, final Handler hProgress, final BitmapFactory.Options bop) {
        if (!localDir.endsWith("/")) {
            localDir += "/";
        }
        File fDir = new File(localDir);
        if (!fDir.exists()) {
            fDir.mkdirs();
        }
        final String filePath = localDir + localFile;

        File fImg = new File(filePath);
        if (fImg.exists()) {
            try {
                if (iv != null) {
                    if (bop != null) {
                        iv.setImageBitmap(BitmapFactory.decodeFile(filePath, bop));
                    } else {
                        iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
                    }
                }
            } catch (Throwable th) {

            }
            return;
        }

        final Handler hImage = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {
                    File file = new File(filePath);
                    if (file.exists()) {
                        try {
                            if (iv != null) {
                                if (bop != null) {
                                    iv.setImageBitmap(BitmapFactory.decodeFile(filePath, bop));
                                } else {
                                    iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
                                }
                            }
                        } catch (Throwable e) {

                        }
                    }
                    for (int i = 0; i < listDownloading.size(); i++) {
                        if (listDownloading.get(i).fileName.equals(filePath)) {
                            listDownloading.remove(i);
                            break;
                        }
                    }
                }
                super.handleMessage(msg);
            }
        };

        final Thread tDownload = new Thread(new Runnable() {
            @Override
            public void run() {
                downloadFile(url, filePath, hProgress);
                hImage.sendEmptyMessage(1);
            }
        });

        DownloadInfo info = new DownloadInfo();
        info.fileName = filePath;
        info.thread = tDownload;

        boolean hasTask = false;
        for (DownloadInfo di : listDownloading) {
            if (di.fileName.equals(info.fileName)) {
                hasTask = true;
                break;
            }
        }
        if (!hasTask) {
            listDownloading.add(info);
            tDownload.start();
        }
    }

    public static void downloadFileT(final Context context, final ImageView iv, final String url, String localDir, final String localFile, final Handler hProgress) {
        downloadFileT(context, iv, url, localDir, localFile, hProgress, null);
    }

    public static void downloadFile(String address, String localFile, Handler h) {

        File fTmp = new File(localFile);
        if (fTmp.exists()) {
            fTmp.delete();
        }

        URL url = null;
        int filesize = 0;
        int position = 0;
        try {
            url = new URL(address);
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            InputStream in = con.getInputStream();
            filesize = con.getContentLength();
            if (h != null) {
                try {
                    Message msg = new Message();
                    msg.what = WHAT_DOWNLOAD_START;
                    msg.arg1 = position;
                    msg.arg2 = filesize;
                    h.sendMessage(msg);
                } catch (Exception e) {

                }
            }
            File fileOut = new File(localFile + ".tmp");
            FileOutputStream out = new FileOutputStream(fileOut);
            byte[] bytes = new byte[1024];
            int c;
            while ((c = in.read(bytes)) != -1) {
                out.write(bytes, 0, c);
                position += c;

                if (h != null) {
                    try {
                        Message msg = new Message();
                        msg.what = WHAT_DOWNLOAD_PROGRESS;
                        msg.arg1 = position;
                        msg.arg2 = filesize;
                        h.sendMessage(msg);
                    } catch (Exception e) {

                    }
                }
            }
            in.close();
            out.close();
            fileOut.renameTo(fTmp);
            if (h != null) {
                try {
                    Message msg = new Message();
                    msg.what = WHAT_DOWNLOAD_FINISH;
                    msg.arg1 = 0;
                    msg.arg2 = filesize;
                    h.sendMessage(msg);
                } catch (Exception e) {

                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

}
