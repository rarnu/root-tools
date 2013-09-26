package com.rarnu.utils.ex;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.widget.ProgressBar;

import java.io.File;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public abstract class DownloaderEx {

    public static final int WHAT_DOWNLOAD_PROGRESS = 1;
    public static final int WHAT_DOWNLOAD_FINISH = 2;
    public static final int WHAT_DOWNLOAD_ERROR = 3;
    private static final int INIT = 1;
    private static final int DOWNLOADING = 2;
    private static final int PAUSE = 3;
    private ProgressBar progressBar;
    private String urlstr;
    private String localfile;
    private int threadcount;
    private Handler mHandler;
    private int fileSize;
    private List<DownloadInfoEx> infos;
    private int state = INIT;
    private Context mContext;

    public DownloaderEx(Context context, String urlstr, String localfile, int threadcount, ProgressBar progressBar, Handler mHandler) {
        this.mContext = context;
        this.urlstr = urlstr;
        this.localfile = localfile;
        this.threadcount = threadcount;
        this.mHandler = mHandler;
        this.progressBar = progressBar;
    }

    public abstract void saveDownloadInfos(Context context, List<DownloadInfoEx> infos);

    public abstract List<DownloadInfoEx> getDownloadInfos(Context context, String url);

    public abstract boolean hasDownloadInfo(Context context, String url);

    public abstract void deleteDownloadInfo(Context context, String url);

    public abstract void updataDownloadInfo(Context context, String url, int threadId, int compeleteSize);

    public boolean isDownloading() {
        return state == DOWNLOADING;
    }

    public ProgressBar getProgressBar() {
        return progressBar;
    }

    public String getUrlStr() {
        return urlstr;
    }

    public void getDownloaderInfors(final LoadInfoCallback callback, final Object callbackObj) {

        final Handler hToMainThread = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {
                    LoadInfoEx info = (LoadInfoEx) msg.obj;
                    if (callback != null) {
                        callback.onLoadInfoReady(DownloaderEx.this, info, callbackObj);
                    }
                }
                super.handleMessage(msg);
            }
        };

        new Thread(new Runnable() {

            @Override
            public void run() {
                LoadInfoEx loadInfo = null;
                if (isFirst()) {
                    init();
                    int range = fileSize / threadcount;
                    infos = new ArrayList<DownloadInfoEx>();
                    for (int i = 0; i < threadcount - 1; i++) {
                        DownloadInfoEx info = new DownloadInfoEx(i, i * range, (i + 1) * range - 1, 0, urlstr);
                        infos.add(info);
                    }
                    DownloadInfoEx info = new DownloadInfoEx(threadcount - 1, (threadcount - 1) * range, fileSize - 1, 0, urlstr);
                    infos.add(info);
                    saveDownloadInfos(mContext, infos);
                    loadInfo = new LoadInfoEx(fileSize, 0, urlstr);

                } else {
                    infos = getDownloadInfos(mContext, urlstr);
                    int size = 0;
                    int compeleteSize = 0;
                    for (DownloadInfoEx info : infos) {
                        compeleteSize += info.compeleteSize;
                        size += info.endPos - info.startPos + 1;
                    }
                    loadInfo = new LoadInfoEx(size, compeleteSize, urlstr);
                }
                Message msg = new Message();
                msg.what = 1;
                msg.obj = loadInfo;
                hToMainThread.sendMessage(msg);

            }
        }).start();

    }

    private void init() {
        try {
            URL url = new URL(urlstr);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setConnectTimeout(30000);
            connection.setRequestMethod("GET");
            fileSize = connection.getContentLength();
            File file = new File(localfile);
            if (!file.exists()) {
                file.createNewFile();
            }
            RandomAccessFile accessFile = new RandomAccessFile(file, "rwd");
            accessFile.setLength(fileSize);
            accessFile.close();
            connection.disconnect();
        } catch (Exception e) {

        }
    }

    private boolean isFirst() {
        return !hasDownloadInfo(mContext, urlstr);
    }

    public void download() {
        if (infos != null) {
            if (state == DOWNLOADING)
                return;
            state = DOWNLOADING;
            for (DownloadInfoEx info : infos) {
                new InnerDownloadThread(info.threadId, info.startPos, info.endPos, info.compeleteSize, info.url).start();
            }
        }
    }

    public void delete(String pkgName) {
        deleteDownloadInfo(mContext, pkgName);
    }

    public void pause() {
        state = PAUSE;
    }

    public void reset() {
        state = INIT;
    }

    public interface LoadInfoCallback {
        void onLoadInfoReady(DownloaderEx downloader, LoadInfoEx info, Object callbackObj);
    }

    public class InnerDownloadThread extends Thread {
        private int threadId;
        private int startPos;
        private int endPos;
        private int compeleteSize;
        private String urlstr;

        public InnerDownloadThread(int threadId, int startPos, int endPos, int compeleteSize, String urlstr) {
            this.threadId = threadId;
            this.startPos = startPos;
            this.endPos = endPos;
            this.compeleteSize = compeleteSize;
            this.urlstr = urlstr;
        }

        @Override
        public void run() {
            HttpURLConnection connection = null;
            RandomAccessFile randomAccessFile = null;
            InputStream is = null;
            try {
                URL url = new URL(urlstr);
                Log.e("MyThread", urlstr);
                connection = (HttpURLConnection) url.openConnection();
                connection.setConnectTimeout(30000);
                connection.setRequestMethod("GET");
                connection.setRequestProperty("Range", "bytes=" + (startPos + compeleteSize) + "-" + endPos);

                randomAccessFile = new RandomAccessFile(localfile, "rwd");
                randomAccessFile.seek(startPos + compeleteSize);
                is = connection.getInputStream();
                byte[] buffer = new byte[4096];
                int length = -1;
                while ((length = is.read(buffer)) != -1) {
                    randomAccessFile.write(buffer, 0, length);
                    compeleteSize += length;
                    updataDownloadInfo(mContext, urlstr, threadId, compeleteSize);
                    Message message = Message.obtain();
                    message.what = WHAT_DOWNLOAD_PROGRESS;
                    message.obj = DownloaderEx.this;
                    message.arg1 = length;
                    mHandler.sendMessage(message);
                    if (state == PAUSE) {
                        break;
                    }
                }

                Message message = Message.obtain();
                message.what = WHAT_DOWNLOAD_FINISH;
                message.obj = DownloaderEx.this;
                message.arg1 = 0;
                mHandler.sendMessage(message);
            } catch (Exception e) {
                Message message = Message.obtain();
                message.what = WHAT_DOWNLOAD_ERROR;
                message.obj = DownloaderEx.this;
                message.arg1 = 0;
                mHandler.sendMessage(message);

            } finally {
                try {
                    is.close();
                    randomAccessFile.close();
                    connection.disconnect();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

        }
    }
}
