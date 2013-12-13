package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.ShareItem;
import com.tencent.mm.sdk.openapi.*;

public class ShareUtils {

    public static final String WX_APPID = "wxe9bd2c3ef21f7122";
    public static final String SHARE_URL = "http://rarnu.7thgen.info/d.php";
    private static IWXAPI api = null;

    public static void init(Context context) {
        if (api == null) {
            api = WXAPIFactory.createWXAPI(context, WX_APPID, false);
        }
    }

    public static void share(Context context, int mode, ShareItem item) {
        switch (mode) {
            case 0:
                // wechat
                shareToWechat(context);
                break;
            case 1:
            case 2:
            case 3:
                shareToAll(context, item.packageName);
                break;
            case 4:
                // mms
                shareToMms(context, item.packageName);
                break;
            case 5:
                // mail
                shareToEmail(context, item.packageName);
                break;

        }
    }

    public static void shareToMms(Context context, String packageName) {
        Intent inMms = new Intent(Intent.ACTION_SENDTO);
        inMms.setPackage(packageName);
        inMms.setData(Uri.parse("smsto:"));
        inMms.putExtra("sms_body", context.getString(R.string.share_body));
        context.startActivity(inMms);
    }

    public static IWXAPI registerToWechat(Context context) {
        init(context);
        api.registerApp(WX_APPID);
        return api;
    }

    public static void shareToWechat(Context context) {
        // use wechat sdk for shareing messages
        init(context);
        WXMediaMessage msg = buildeWechatMessage(context);
        SendMessageToWX.Req req = new SendMessageToWX.Req();
        req.transaction = "webpage" + System.currentTimeMillis();
        req.message = msg;
        req.scene = SendMessageToWX.Req.WXSceneSession;
        api.sendReq(req);
    }

    public static void responseToWechat(Context context, Bundle bn) {
        init(context);
        WXMediaMessage msg = buildeWechatMessage(context);
        GetMessageFromWX.Resp resp = new GetMessageFromWX.Resp();
        resp.transaction = new GetMessageFromWX.Req(bn).transaction;
        resp.message = msg;
        api.sendResp(resp);
    }

    private static WXMediaMessage buildeWechatMessage(Context context) {
        WXWebpageObject webpage = new WXWebpageObject();
        webpage.webpageUrl = SHARE_URL;
        WXMediaMessage msg = new WXMediaMessage(webpage);
        msg.title = context.getString(R.string.share_title);
        msg.description = context.getString(R.string.share_body);
        return msg;
    }

    public static void shareToAll(Context context, String packageName) {
        Intent shareIntent = buildShareIntent(context, packageName);
        context.startActivity(shareIntent);
    }

    public static void shareToEmail(Context context, String packageName) {
        Intent shareIntent = buildShareIntent(context, packageName);
        shareIntent.putExtra(Intent.EXTRA_SUBJECT, context.getString(R.string.share_title));
        context.startActivity(shareIntent);
    }

    private static Intent buildShareIntent(Context context, String packageName) {
        Intent shareIntent = new Intent(Intent.ACTION_SEND);
        shareIntent.setType("image/*");
        shareIntent.putExtra(Intent.EXTRA_TEXT, context.getString(R.string.share_body));
        shareIntent.setPackage(packageName);
        return shareIntent;
    }
}
