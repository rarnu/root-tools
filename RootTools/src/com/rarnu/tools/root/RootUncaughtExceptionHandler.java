package com.rarnu.tools.root;

import android.content.Context;
import android.os.Looper;
import android.widget.Toast;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.utils.AccountUtils;
import com.rarnu.utils.DeviceUtils;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;

public class RootUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

    private Context context;

    public RootUncaughtExceptionHandler(Context ctx) {
        this.context = ctx;
    }

    @Override
    public void uncaughtException(Thread thread, final Throwable ex) {

        new Thread(new Runnable() {

            @Override
            public void run() {
                String msg = ex.getMessage();
                String transErr = getErrorMessage(ex);
                sendReport(msg, transErr);
                Looper.prepare();
                Toast.makeText(context, R.string.crash_terminate, Toast.LENGTH_LONG).show();
                Looper.loop();
            }
        }).start();
        try {
            Thread.sleep(3000);
        } catch (InterruptedException e) {

        }

        System.exit(0);
    }

    private String getErrorMessage(Throwable ex) {
        PrintWriter pw = null;
        Writer writer = null;
        String error = "";
        try {
            writer = new StringWriter();
            pw = new PrintWriter(writer);
            ex.printStackTrace(pw);
            error = writer.toString();
            error = error.replace("\n", "\n<br>");
        } catch (Exception e) {

        } finally {
            if (pw != null) {
                try {
                    pw.close();
                } catch (Exception e) {

                }
            }
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e) {

                }
            }
        }

        return error;
    }

    private void sendReport(String msg, String stack) {
        MobileApi.sendCrashLog(
                DeviceUtils.getDeviceUniqueId(context),
                GlobalInstance.device.roProductModel,
                GlobalInstance.device.roBuildVersionSdk,
                AccountUtils.getBindedEmailAddress(context),
                GlobalInstance.device.roBuildDescription,
                msg + "\n<br>" + stack,
                DeviceUtils.getAppVersionName(context));
    }
}
