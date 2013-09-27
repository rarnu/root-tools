package com.yugioh.android.utils;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.Intent;
import android.net.Uri;
import android.widget.ImageButton;
import android.widget.ImageView;
import com.rarnu.utils.DownloadUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.RecommandInfo;
import com.yugioh.android.define.NetworkDefine;
import com.yugioh.android.define.PathDefine;

public class RecommandUtils {

    public static void showUrlRecommand(Context context, RecommandInfo info) {
        if (info.bigQR.equals("")) {
            jumpUrl(context, info);
        } else {
            jumpUrlQr(context, info);
        }
    }

    public static void showTextRecommand(Context context, RecommandInfo info) {
        // show dialog window and QR if it has.
        AlertDialog.Builder ab = new AlertDialog.Builder(context);
        ab.setTitle(info.name);
        ab.setMessage(info.jumpText);
        if (!info.bigQR.equals("")) {
            ImageView ivQr = new ImageButton(context);
            ivQr.setBackgroundDrawable(null);
            ab.setView(ivQr);
            DownloadUtils.downloadFileT(context, ivQr, NetworkDefine.RECOMMAND_IMAGE_URL + info.bigQR, PathDefine.RECOMMAND_PATH, info.bigQR, null);
        }
        ab.setPositiveButton(R.string.ok, null);
        ab.show();

    }

    private static void jumpUrl(Context context, RecommandInfo info) {
        Intent inJump = new Intent(Intent.ACTION_VIEW);
        inJump.setData(Uri.parse(info.jumpUrl));
        context.startActivity(inJump);
    }

    private static void jumpUrlQr(final Context context, final RecommandInfo info) {
        AlertDialog.Builder ab = new AlertDialog.Builder(context);
        ab.setTitle(info.name);
        ab.setMessage(info.jumpText);

        ImageView ivQr = new ImageButton(context);
        ivQr.setBackgroundDrawable(null);
        ab.setView(ivQr);
        DownloadUtils.downloadFileT(context, ivQr, NetworkDefine.RECOMMAND_IMAGE_URL + info.bigQR, PathDefine.RECOMMAND_PATH, info.bigQR, null);

        ab.setPositiveButton(R.string.jump_jump, new OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                jumpUrl(context, info);
            }
        });
        ab.show();
    }
}
