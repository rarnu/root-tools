package com.sbbs.me.android.dialog;

import android.app.Dialog;
import android.content.Context;
import android.net.Uri;
import android.util.Log;
import android.view.Window;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.WebSettings.LayoutAlgorithm;

import com.sbbs.me.android.R;
import com.sbbs.me.android.utils.Config;
import com.sbbs.me.android.utils.GoogleOAuth;

public class GoogleOAuthDialog extends Dialog {

	WebView wvGoogleAuth;
	private Context mContext;
	private String clientId;
	private String callbackUrl;
	private GoogleOAuth oauth;

	String accountUrl = "https://accounts.google.com/o/oauth2/auth?client_id=%s&response_type=token&redirect_uri=%s&scope=https://www.googleapis.com/auth/userinfo.email+https://www.googleapis.com/auth/userinfo.profile";

	public GoogleOAuthDialog(Context context, String clientId,
			String callbackUrl, GoogleOAuth oauth) {

		super(context);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.dialog_google_oauth);
		setCanceledOnTouchOutside(false);
		this.oauth = oauth;
		this.mContext = context;
		this.clientId = clientId;
		this.callbackUrl = callbackUrl;

		wvGoogleAuth = (WebView) findViewById(R.id.wvGoogleAuth);
		wvGoogleAuth.getSettings().setAppCacheEnabled(true);
		wvGoogleAuth.getSettings().setJavaScriptEnabled(true);
		wvGoogleAuth.getSettings().setLayoutAlgorithm(
				LayoutAlgorithm.SINGLE_COLUMN);
		wvGoogleAuth.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(WebView view, String url) {
				Log.e("shouldOverrideUrlLoading", url);
				if (url.startsWith("http://sbbs.me/auth/google_oauth2/callback")) {
					Config.setAccountType(mContext, 0);
					parseUrl(url);
					if (isShowing()) {
						dismiss();
					}
				}
				return super.shouldOverrideUrlLoading(view, url);
			}
		});

		wvGoogleAuth.loadUrl(String.format(accountUrl, this.clientId,
				this.callbackUrl));
	}

	private void parseUrl(String url) {
		url = url.replace("#", "?");
		Uri uri = Uri.parse(url);
		String access_token = uri.getQueryParameter("access_token");
		// String token_type = uri.getQueryParameter("token_type");
		// String expires_in = uri.getQueryParameter("expires_in");

		oauth.getGoogleUserInfo(access_token);
	}

}
