package com.sbbs.me.android.dialog;

import com.sbbs.me.android.R;
import com.sbbs.me.android.utils.Config;
import com.sbbs.me.android.utils.GithubOAuth;

import android.app.Dialog;
import android.content.Context;
import android.net.Uri;
import android.util.Log;
import android.view.Window;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.WebSettings.LayoutAlgorithm;

public class GithubOAuthDialog extends Dialog {

	WebView wvGithubAuth;
	private Context mContext;
	private String clientId;
	private String callbackUrl;
	private GithubOAuth oauth;

	String accessUrl = "https://github.com/login/oauth/authorize?client_id=%s&redirect_uri=%s&scope=user";
	
	public GithubOAuthDialog(Context context, String clientId,
			String callbackUrl, GithubOAuth oauth) {
		super(context);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.dialog_github_oauth);
		setCanceledOnTouchOutside(false);
		this.oauth = oauth;
		this.mContext = context;
		this.clientId = clientId;
		this.callbackUrl = callbackUrl;
		
		wvGithubAuth = (WebView) findViewById(R.id.wvGithubAuth);
		wvGithubAuth.getSettings().setAppCacheEnabled(true);
		wvGithubAuth.getSettings().setJavaScriptEnabled(true);
		wvGithubAuth.getSettings().setLayoutAlgorithm(LayoutAlgorithm.SINGLE_COLUMN);
		wvGithubAuth.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(WebView view, String url) {
				Log.e("shouldOverrideUrlLoading", url);
				if (url.startsWith("http://sbbs.me/auth/github_oauth2/callback")) {
					Config.setAccountType(mContext, 1);
					parseUrl(url);
					if (isShowing()) {
						dismiss();
					}
				}
				return super.shouldOverrideUrlLoading(view, url);
			}
		});
		wvGithubAuth.loadUrl(String.format(accessUrl, this.clientId,
				this.callbackUrl));
	}
	
	private void parseUrl(String url) {
		url = url.replace("#", "?");
		Uri uri = Uri.parse(url);
		String code = uri.getQueryParameter("code");
		oauth.getGithubUserInfo(code);
	}
}
