package com.rarnu.tools.neo.utils;

import android.graphics.Bitmap;

import java.io.File;
import java.io.FileOutputStream;

/**
 * Created by rarnu on 11/21/16.
 */
public class ImageUtils {

    public static void saveBitmapToFile(Bitmap bmp, String fileName) {
        if (fileName != null && !fileName.equals("")) {
            try {
                File f = new File(fileName);
                f.createNewFile();
                FileOutputStream fOut = null;
                fOut = new FileOutputStream(f);
                bmp.compress(Bitmap.CompressFormat.PNG, 100, fOut);
                fOut.flush();
                fOut.close();
            } catch (Exception e) {

            }
        }
    }

}
