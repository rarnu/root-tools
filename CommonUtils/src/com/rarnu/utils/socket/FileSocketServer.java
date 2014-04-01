package com.rarnu.utils.socket;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Random;

public class FileSocketServer {

    private boolean receiving = false;
    private ServerSocket server = null;
    private String savePath = "";
    private int port;
    private SocketServerCallback callback;

    public FileSocketServer(final SocketServerCallback callback, int port, String savePath) {
        this.savePath = savePath;
        this.port = port;
        this.callback = callback;
    }

    public void startListen() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    receiving = true;
                    server = new ServerSocket(port);
                    while (receiving) {
                        Socket client = server.accept();
                        doReceiveFile(client);
                        client.close();
                    }
                } catch (Exception e) {
                    if (callback != null) {
                        callback.onError(e.getMessage());
                    }
                }
            }
        }).start();
    }

    private void doReceiveFile(Socket client) {
        try {
            int randomId = new Random(System.currentTimeMillis()).nextInt(65536);
            DataInputStream dis = new DataInputStream(client.getInputStream());
            int bufferSize = 1024;
            byte[] buf = new byte[bufferSize];
            long passedlen = 0L;
            String savePathReal = savePath + dis.readUTF();
            DataOutputStream fileOut = new DataOutputStream(new FileOutputStream(savePathReal));
            long len = dis.readLong();
            if (callback != null) {
                callback.onReceiveFile(randomId, savePathReal, len, 0L, 0);
            }
            while (true) {
                int read = 0;
                if (dis != null) {
                    read = dis.read(buf);
                }
                passedlen += read;
                if (read == -1) {
                    break;
                }
                fileOut.write(buf, 0, read);
                if (callback != null) {
                    callback.onReceiveFile(randomId, savePathReal, len, passedlen, 2);
                }
            }
            dis.close();
            fileOut.close();
            if (callback != null) {
                callback.onReceiveFile(randomId, savePathReal, len, len, 1);
            }
        } catch (Exception e) {
            if (callback != null) {
                callback.onError(e.getMessage());
            }
        }
    }

    public void stopListen() {
        receiving = false;
        try {
            if (server != null) {
                server.close();
            }
        } catch (Exception e) {
        }
    }

}
