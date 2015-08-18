package com.rarnu.devlib.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.webkit.CookieSyncManager;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

/**
 * Created by rarnu on 4/16/15.
 */
public class WebViewEx extends WebView {

    private final static String TYPE_NAME = "rarnu_android";

    public WebViewEx(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public WebViewEx(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public WebViewEx(Context context) {
        super(context);
        init();
    }

    private void init() {
        setVerticalScrollBarEnabled(false);
        setHorizontalScrollBarEnabled(false);
        setScrollBarStyle(View.SCROLLBARS_INSIDE_OVERLAY);

        WebSettings ws = getSettings();
        ws.setAllowFileAccess(true);
        ws.setCacheMode(WebSettings.LOAD_NO_CACHE);
        ws.setLoadWithOverviewMode(true);
        ws.setJavaScriptEnabled(true);
        ws.setJavaScriptCanOpenWindowsAutomatically(true);
        ws.setUserAgentString(ws.getUserAgentString() + TYPE_NAME);

        ws.setDatabaseEnabled(true);
        ws.setDomStorageEnabled(true);
        CookieSyncManager cookieSyncManager = CookieSyncManager.createInstance(getContext());
        cookieSyncManager.sync();

        setWebViewClient(new WebViewClient() {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                view.loadUrl(url);
                return true;
            }
        });
    }
}
