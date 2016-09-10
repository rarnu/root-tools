package com.rarnu.tools.neo.root;

import android.content.Context;
import android.os.Build;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStreamReader;

public class RootUtils {

    private static boolean _isrejected = false;

    public static boolean isRejected() {
        return _isrejected;
    }

    public static boolean isHasSu() {
        return new File("/system/bin/su").exists() || new File("/system/xbin/su").exists();
    }

    public static CommandResult runCommand(String command, boolean root) {
        return runCommand(new String[]{command}, root);
    }

    public static CommandResult runCommand(String[] prog) {
        return runCommand(prog, false);
    }

    public static CommandResult runCommand(String[] command, boolean root) {

        Process process = null;
        DataOutputStream os = null;
        BufferedReader brOut = null;
        BufferedReader brErr = null;

        CommandResult ret = new CommandResult();
        try {
            StringBuilder output = new StringBuilder();
            StringBuilder error = new StringBuilder();
            if (root) {
                process = Runtime.getRuntime().exec("su");
                os = new DataOutputStream(process.getOutputStream());
                for (String s : command) {
                    os.writeBytes(s + "\n");
                }
                os.writeBytes("exit\n");
                os.flush();
            } else {
                process = Runtime.getRuntime().exec(command);
            }
            String line;
            brOut = new BufferedReader(new InputStreamReader(process.getInputStream()));
            while ((line = brOut.readLine()) != null) {
                output.append(line).append('\n');
            }
            brErr = new BufferedReader(new InputStreamReader(process.getErrorStream()));
            while ((line = brErr.readLine()) != null) {
                error.append(line).append('\n');
            }
            process.waitFor();
            ret.result = output.toString().trim();
            ret.error = error.toString().trim();

            // ignore warning output
            if (ret.error.startsWith("WARNING:")) {
                ret.error = "";
            }
        } catch (Exception e) {
            ret.result = "";
            ret.error = e.getMessage();
        } finally {
            try {
                if (os != null) {
                    os.close();
                }
                if (brOut != null) {
                    brOut.close();
                }
                if (brErr != null) {
                    brErr.close();
                }
            } catch (Exception e) {
                ret.result = "";
                ret.error = e.getMessage();
            }
        }
        return ret;
    }

    public static void mountRW() {
        String cmd = "mount -o remount,rw /system"; // buildMountCommand();
        _isrejected = false;
        CommandResult ret = runCommand(cmd, true);
        if (!ret.error.equals("") && (ret.error.toLowerCase().contains("denied")
                || (ret.error.toLowerCase().contains("null environment"))
                || ret.error.toLowerCase().contains("not allowed"))) {
            _isrejected = true;
        }
    }

    public static void makePreferenceReadable(Context ctx) {
        if (Build.VERSION.SDK_INT >= 24) {
            RootUtils.runCommand(String.format("chmod -R 777 /data/data/%s/shared_prefs", ctx.getPackageName()), true);
        }
    }

}
