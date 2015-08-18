package com.rarnu.devlib.component;

import java.util.concurrent.atomic.AtomicBoolean;

import android.content.Context;
import android.util.AttributeSet;
import android.webkit.WebView;


public class PullToRefreshWebView2 extends PullToRefreshWebView {

	static final String JS_INTERFACE_PKG = "ptr";
	static final String DEF_JS_READY_PULL_DOWN_CALL = "javascript:isReadyForPullDown();";
	static final String DEF_JS_READY_PULL_UP_CALL = "javascript:isReadyForPullUp();";

	public PullToRefreshWebView2(Context context) {
		super(context);
	}

	public PullToRefreshWebView2(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PullToRefreshWebView2(Context context, Mode mode) {
		super(context, mode);
	}

	private JsValueCallback mJsCallback;
	private final AtomicBoolean mIsReadyForPullDown = new AtomicBoolean(false);
	private final AtomicBoolean mIsReadyForPullUp = new AtomicBoolean(false);

	@Override
	protected WebView createRefreshableView(Context context, AttributeSet attrs) {
		WebView webView = super.createRefreshableView(context, attrs);

		mJsCallback = new JsValueCallback();
		webView.addJavascriptInterface(mJsCallback, JS_INTERFACE_PKG);

		return webView;
	}

	@Override
	protected boolean isReadyForPullStart() {
		getRefreshableView().loadUrl(DEF_JS_READY_PULL_DOWN_CALL);

		return mIsReadyForPullDown.get();
	}

	@Override
	protected boolean isReadyForPullEnd() {
		getRefreshableView().loadUrl(DEF_JS_READY_PULL_UP_CALL);
		return mIsReadyForPullUp.get();
	}

	final class JsValueCallback {

		public void isReadyForPullUpResponse(boolean response) {
			mIsReadyForPullUp.set(response);
		}
		public void isReadyForPullDownResponse(boolean response) {
			mIsReadyForPullDown.set(response);
		}
	}
}
