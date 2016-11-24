package com.rarnu.tools.neo.utils;

import android.util.Log;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;

public class RootUtils {

    public static class CommandResult {
        public String result = "";
        public String error = "";
    }

    interface CommandCallback {
        void onReadLine(String line);

        void onReadError(String line);

        void onCommandFinish();
    }

    public static CommandResult runCommand(String command, boolean root) {
        return runCommand(new String[]{command}, root, null);
    }

    public static CommandResult runCommand(String[] prog) {
        return runCommand(prog, false, null);
    }

    public static CommandResult runCommand(String[] prog, CommandCallback callback) {
        return runCommand(prog, false, callback);
    }

    public static CommandResult runCommand(String command, boolean root, CommandCallback callback) {
        return runCommand(new String[]{command}, root, callback);
    }

    public static CommandResult runCommand(String[] command, boolean root, CommandCallback callback) {
        Log.e("RootUtils", command[0]);
        Process process = null;
        DataOutputStream os = null;
        BufferedReader brOut = null;
        BufferedReader brErr = null;
        CommandResult ret = new CommandResult();
        try {
            StringBuffer output = new StringBuffer();
            StringBuffer error = new StringBuffer();
            if (root) {
                process = Runtime.getRuntime().exec("su");
                os = new DataOutputStream(process.getOutputStream());

                os.writeBytes(command[0] + "\n");
                os.writeBytes("exit\n");
                os.flush();
            } else {
                if (command.length == 1) {
                    process = Runtime.getRuntime().exec(command[0]);
                } else {
                    process = Runtime.getRuntime().exec(command);
                }
            }

            String line;
            brOut = new BufferedReader(new InputStreamReader(process.getInputStream()));
            while ((line = brOut.readLine()) != null) {
                output.append(line).append('\n');
                if (callback != null) {
                    callback.onReadLine(line);
                }
            }
            brErr = new BufferedReader(new InputStreamReader(process.getErrorStream()));
            while ((line = brErr.readLine()) != null) {
                error.append(line).append('\n');
                if (callback != null) {
                    callback.onReadError(line);
                }
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
        if (callback != null) {
            callback.onCommandFinish();
        }
        return ret;
    }

}
