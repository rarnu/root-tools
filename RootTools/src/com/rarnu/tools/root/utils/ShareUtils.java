package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.ShareItem;
import com.tencent.mm.sdk.openapi.*;

public class ShareUtils {

    public static void share(Context context, int mode, ShareItem item, int text) {
        switch (mode) {
            case 0:
                // wechat
                shareToWechat(context, text);
                break;
            case 1:
            case 2:
            case 3:
                shareToAll(context, item.packageName, text);
                break;
            case 4:
                // mms
                shareToMms(context, item.packageName, text);
                break;
            case 5:
                // mail
                shareToEmail(context, item.packageName, text);
                break;

        }
    }

    public static void shareToMms(Context context, String packageName, int res) {
        Intent inMms = new Intent(Intent.ACTION_SENDTO);
        inMms.setPackage(packageName);
        inMms.setData(Uri.parse("smsto:"));
        inMms.putExtra("sms_body", context.getString(res));
        context.startActivity(inMms);
    }

    public static void shareToWechat(Context context, int res) {
        // use wechat sdk for shareing messages
        IWXAPI api = WXAPIFactory.createWXAPI(context, "wxe9bd2c3ef21f7122");

        WXWebpageObject webpage = new WXWebpageObject();
        webpage.webpageUrl = "http://rarnu.7thgen.info/d.php";
        WXMediaMessage msg = new WXMediaMessage(webpage);
        msg.title = context.getString(R.string.share_title);
        msg.description = context.getString(R.string.share_body);

        SendMessageToWX.Req req = new SendMessageToWX.Req();
        req.transaction = "webpage" + System.currentTimeMillis();
        req.message = msg;
        req.scene = SendMessageToWX.Req.WXSceneSession;
        api.sendReq(req);

    }

    public static void shareToAll(Context context, String packageName, int res) {
        Intent shareIntent = buildShareIntent(context, packageName, res);
        context.startActivity(shareIntent);
    }

    public static void shareToEmail(Context context, String packageName, int res) {
        Intent shareIntent = buildShareIntent(context, packageName, res);
        shareIntent.putExtra(Intent.EXTRA_SUBJECT, context.getString(R.string.share_title));
        context.startActivity(shareIntent);
    }

    private static Intent buildShareIntent(Context context, String packageName, int res) {
        Intent shareIntent = new Intent(Intent.ACTION_SEND);
        shareIntent.setType("image/*");
        shareIntent.putExtra(Intent.EXTRA_TEXT, context.getString(R.string.share_body));
        shareIntent.setPackage(packageName);
        return shareIntent;
    }
}
