package com.rarnu.tools.root.wxapi;

import android.app.Activity;
import android.os.Bundle;
import com.rarnu.tools.root.utils.ShareUtils;
import com.tencent.mm.sdk.openapi.*;

public class WXEntryActivity extends Activity implements IWXAPIEventHandler {
    private IWXAPI api;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        api = WXAPIFactory.createWXAPI(this, ShareUtils.WX_APPID, false);
        api.handleIntent(getIntent(), this);
    }

    @Override
    public void onReq(BaseReq req) {
        switch (req.getType()) {
            case ConstantsAPI.COMMAND_GETMESSAGE_FROM_WX:
                ShareUtils.responseToWechat(this, getIntent().getExtras());
                finish();
                break;
        }
    }

    @Override
    public void onResp(BaseResp resp) {

    }

}