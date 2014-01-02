package com.rarnu.tools.root.utils;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.ShareItem;
import com.tencent.mm.sdk.openapi.*;
import com.tencent.tauth.IUiListener;
import com.tencent.tauth.Tencent;
import com.tencent.tauth.UiError;
import org.json.JSONObject;

import java.util.ArrayList;

public class ShareUtils {

    public static final String WX_APPID = "wxe9bd2c3ef21f7122";
    public static final String TENCENT_APPID = "101000413";
    public static final String SHARE_URL = "http://rarnu.7thgen.info/d.php";
    public static final String ICON_URL = "http://rarnu.7thgen.info/icon.png";
    private static IWXAPI api = null;
    private static Tencent tencent = null;

    private static IUiListener tencentListener = new IUiListener() {
        @Override
        public void onComplete(JSONObject jsonObject) {

        }

        @Override
        public void onError(UiError uiError) {

        }

        @Override
        public void onCancel() {

        }
    };

    public static void init(Context context) {
        if (api == null) {
            api = WXAPIFactory.createWXAPI(context, WX_APPID, false);
        }
    }

    public static void share(Context context, ShareItem item) {
        switch (item.id) {
            case 0:
                // wechat
                shareToWechat(context);
                break;
            case 1:
                // timeline
                shareToTimeline(context);
                break;
            case 2:
                // qq
                shareToQQ((Activity) context);
                break;
            case 3:
                // qzone
                shareToQZone((Activity) context);
                break;
            case 4:
            case 5:
            case 6:
                shareToAll(context, item.packageName);
                break;
            case 7:
                // mms
                shareToMms(context, item.packageName);
                break;
            case 8:
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

    public static void shareToTimeline(Context context) {
        // use wechat sdk for shareing messages
        init(context);
        WXMediaMessage msg = buildeWechatMessage(context);
        SendMessageToWX.Req req = new SendMessageToWX.Req();
        req.transaction = "webpage" + System.currentTimeMillis();
        req.message = msg;
        req.scene = SendMessageToWX.Req.WXSceneTimeline;
        if (api != null) {
            api.sendReq(req);
        }
    }

    public static void responseToWechat(Context context, Bundle bn) {
        init(context);
        WXMediaMessage msg = buildeWechatMessage(context);
        GetMessageFromWX.Resp resp = new GetMessageFromWX.Resp();
        resp.transaction = new GetMessageFromWX.Req(bn).transaction;
        resp.message = msg;
        if (api != null) {
            api.sendResp(resp);
        }
    }

    private static WXMediaMessage buildeWechatMessage(Context context) {
        WXWebpageObject webpage = new WXWebpageObject();
        webpage.webpageUrl = SHARE_URL;
        WXMediaMessage msg = new WXMediaMessage(webpage);
        msg.title = context.getString(R.string.share_title);
        msg.description = context.getString(R.string.share_body);
        return msg;
    }

    public static void shareToQQ(Activity context) {
        initTencent(context);
        if (tencent != null) {
            Bundle bn = buildQQMessage(context);
            bn.putString(Tencent.SHARE_TO_QQ_IMAGE_URL, ICON_URL);
            tencent.shareToQQ(context, bn, tencentListener);
        }
    }

    public static void shareToQZone(Activity context) {
        initTencent(context);
        if (tencent != null) {
            Bundle bn = buildQQMessage(context);
            ArrayList<String> list = new ArrayList<String>();
            list.add(ICON_URL);
            bn.putStringArrayList(Tencent.SHARE_TO_QQ_IMAGE_URL, list);
            tencent.shareToQzone(context, bn, tencentListener);
        }
    }

    private static Bundle buildQQMessage(Context context) {
        Bundle bundle = new Bundle();
        bundle.putInt(Tencent.SHARE_TO_QQ_KEY_TYPE, Tencent.SHARE_TO_QQ_TYPE_DEFAULT);
        bundle.putString(Tencent.SHARE_TO_QQ_TITLE, context.getString(R.string.share_title));
        bundle.putString(Tencent.SHARE_TO_QQ_SUMMARY, context.getString(R.string.share_body));
        bundle.putString(Tencent.SHARE_TO_QQ_TARGET_URL, SHARE_URL);
        bundle.putString(Tencent.SHARE_TO_QQ_APP_NAME, context.getString(R.string.app_name));

        return bundle;
    }

    private static void initTencent(Activity context) {
        if (tencent == null) {
            tencent = Tencent.createInstance(TENCENT_APPID, context.getApplicationContext());
        }
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
