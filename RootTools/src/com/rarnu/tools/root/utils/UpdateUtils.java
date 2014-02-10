package com.rarnu.tools.root.utils;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.fragmentactivity.UpdateActivity;

public class UpdateUtils {

    public static void showUpdateInfo(final Context context, boolean showDialog) {
        if (context != null) {
            if (GlobalInstance.updateInfo == null
                    || GlobalInstance.updateInfo.result == 0) {
                noUpdate(context);
            } else {
                if (showDialog) {
                    showUpdate(context);
                } else {
                    openUpdateActivity(context);
                }
            }
        }
    }

    private static void noUpdate(Context context) {
        new AlertDialog.Builder(context)
                .setTitle(R.string.check_update)
                .setMessage(R.string.no_update_found)
                .setCancelable(false)
                .setPositiveButton(R.string.ok, null)
                .show();
    }

    private static void showUpdate(final Context context) {
        if (context != null) {
            String msg = String.format(context.getString(R.string.update_found_info), GlobalInstance.updateInfo.versionName, GlobalInstance.updateInfo.size);
            new AlertDialog.Builder(context)
                    .setTitle(R.string.check_update)
                    .setCancelable(false)
                    .setMessage(msg)
                    .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            openUpdateActivity(context);
                        }
                    })
                    .setNegativeButton(R.string.cancel, null)
                    .show();
        }
    }

    private static void openUpdateActivity(Context context) {
        Intent inUpdate = new Intent(context, UpdateActivity.class);
        context.startActivity(inUpdate);
    }

}
