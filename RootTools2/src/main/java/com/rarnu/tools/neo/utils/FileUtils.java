package com.rarnu.tools.neo.utils;

import android.content.Context;
import android.os.Handler;
import android.os.Message;

import java.io.*;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

public class FileUtils {

    public static boolean copyAssetFile(Context context, String fileName, String saveDir) {
        File fAsset = new File(saveDir);
        if (!fAsset.exists()) {
            fAsset.mkdirs();
        }
        try {
            byte[] buffer = new byte[8192];
            File dest = new File(saveDir, fileName);
            if (dest.exists()) {
                dest.delete();
            }
            InputStream is = context.getAssets().open(fileName);
            OutputStream fos = new BufferedOutputStream(new FileOutputStream(dest));
            int n;
            while ((n = is.read(buffer, 0, buffer.length)) != -1) {
                fos.write(buffer, 0, n);
            }
            is.close();
            fos.close();
            return true;
        } catch (Exception ex) {
            return false;
        }
    }

}
