package com.rarnu.utils;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.widget.ImageView;
import com.rarnu.utils.common.DownloadInfo;
import com.rarnu.utils.os.BreakableThread;

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
        for (DownloadInfo di : listDownloading) {
            if (di.fileName.equals(filePath)) {
                try {
                    di.thread.wait(5000);
                    di.thread.interrupt();
                } catch (Exception e) {

                }
                listDownloading.remove(di);
                break;
            }
        }
    }

    public static void downloadFileT(final Context context, final ImageView iv, final String url, String localDir, final String localFile, final Handler hProgress, final BitmapFactory.Options bop, final boolean isRound, final int radis) {
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
                    Bitmap bmp = null;
                    if (bop != null) {
                        bmp = BitmapFactory.decodeFile(filePath, bop);
                    } else {
                        bmp = BitmapFactory.decodeFile(filePath);
                    }
                    if (bmp != null && isRound) {
                        bmp = ImageUtils.roundedCornerBitmap(bmp, radis);
                    }
                    iv.setImageBitmap(bmp);
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
                                Bitmap bmp = null;
                                if (bop != null) {
                                    bmp = BitmapFactory.decodeFile(filePath, bop);
                                } else {
                                    bmp = BitmapFactory.decodeFile(filePath);
                                }
                                if (bmp != null && isRound) {
                                    bmp = ImageUtils.roundedCornerBitmap(bmp, radis);
                                }
                                iv.setImageBitmap(bmp);
                            }
                        } catch (Throwable e) {

                        }
                    }
                    for (DownloadInfo di : listDownloading) {
                        if (di.fileName.equals(filePath)) {
                            listDownloading.remove(di);
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
                downloadFile(url, filePath, hProgress, null);
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
        downloadFileT(context, iv, url, localDir, localFile, hProgress, null, false, 0);
    }

    public static void downloadFileT(final Context context, final ImageView iv, final String url, String localDir, final String localFile, final Handler hProgress, final boolean isRound, final int radis) {
        downloadFileT(context, iv, url, localDir, localFile, hProgress, null, isRound, radis);
    }

    public static void downloadFile(String address, String localFile, Handler h, BreakableThread.RunningCallback callback) {

        File fTmp = new File(localFile);
        if (fTmp.exists()) {
            fTmp.delete();
        }

        boolean isDownloadNormal = true;

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
                if (callback != null) {
                    if (!callback.getRunningState()) {
                        isDownloadNormal = false;
                        break;
                    }
                }
            }
            in.close();
            out.close();
            fileOut.renameTo(fTmp);
            if (!isDownloadNormal) {
                try {
                    fileOut.delete();
                } catch (Exception e) {

                }
                try {
                    fTmp.delete();
                } catch (Exception e) {

                }
            }
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
            Log.e("download error", e.getMessage());
            if (h != null) {
                try {
                    Message msg = new Message();
                    msg.what = WHAT_DOWNLOAD_FINISH;
                    msg.arg1 = 0;
                    msg.arg2 = filesize;
                    h.sendMessage(msg);
                } catch (Exception ex) {

                }
            }
        }

    }

}
