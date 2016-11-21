package com.rarnu.tools.neo.utils;

import android.util.Log;

import java.io.DataOutputStream;

/**
 * Created by rarnu on 11/21/16.
 */
public class RootUtils {

    public static void requestRoot() {
        DataOutputStream os = null;
        try {
            Process process = Runtime.getRuntime().exec("su");
            os = new DataOutputStream(process.getOutputStream());
            os.writeBytes("exit\n");
            os.flush();
            process.waitFor();
        } catch (Exception e) {
            Log.e("RootUtils", "error: " + e.toString());
        } finally {
            try {
                if (os != null) {
                    os.close();
                }
            } catch (Exception e) {
            }
        }
    }

}
