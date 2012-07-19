package com.snda.gyue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.PreferenceManager;
import android.text.Html.ImageGetter;
import android.view.View;
import android.view.View.OnClickListener;
import android.webkit.WebSettings;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.gyue.network.FakeClick;
import com.snda.gyue.utils.FileUtils;

public class ViewArticleActivity extends Activity implements OnClickListener {

	Button btnBack;
	ProgressBar pbRefreshing;
	WebView tvArticle;
	TextView tvTitle, tvDate;
	ScrollView layContent;
	TextView tvSeeWeb;
	RelativeLayout layLoading;
	ImageGetter iGetter;
	ImageView imgShareTencent, imgShareSina;

	boolean inProgress = false;
	boolean tmrEd = false;

	int fontSize = 16;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.view_article);

		btnBack = (Button) findViewById(R.id.btnBack);
		pbRefreshing = (ProgressBar) findViewById(R.id.pbRefreshing);
		tvArticle = (WebView) findViewById(R.id.tvArticle);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
		tvDate = (TextView) findViewById(R.id.tvDate);
		layContent = (ScrollView) findViewById(R.id.layContent);
		tvSeeWeb = (TextView) findViewById(R.id.tvSeeWeb);
		layLoading = (RelativeLayout) findViewById(R.id.layLoading);
		imgShareTencent = (ImageView) findViewById(R.id.imgShareTencent);
		imgShareSina = (ImageView) findViewById(R.id.imgShareSina);

		WebSettings settings = tvArticle.getSettings();
		settings.setLoadWithOverviewMode(false);
		settings.setSupportZoom(false);
		settings.setAllowFileAccess(true);
		settings.setBuiltInZoomControls(false);
		settings.setLayoutAlgorithm(LayoutAlgorithm.SINGLE_COLUMN);

		tvArticle.setVerticalScrollBarEnabled(true);
		tvArticle.setHorizontalScrollBarEnabled(false);
		tvArticle.setFocusable(false);
		tvArticle.setFocusableInTouchMode(false);

		btnBack.setOnClickListener(this);
		tvSeeWeb.setOnClickListener(this);
		imgShareSina.setOnClickListener(this);
		imgShareTencent.setOnClickListener(this);
//		btnZoomIn.setOnClickListener(this);
//		btnZoomOut.setOnClickListener(this);

		// TextSize
		fontSize = PreferenceManager.getDefaultSharedPreferences(this).getInt("font-size", 16);

		tvTitle.setText(GlobalInstance.currentArticle.getTitle());
		tvDate.setText(GlobalInstance.currentArticle.getDate());
		// tvArticle.setTextSize(fontSize);
		
		try {
			FileUtils.createFile(GlobalInstance.currentArticle.getRead().getAbsolutePath(), "");
		} catch (IOException e) {
		}

		setTextView();
		FakeClick.doFakeClick(GlobalInstance.currentArticle.getUid());
	}

	private void setTextView() {
		tmrEd = false;
		final Handler hTmr = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tmrEd = true;
					if (!inProgress) {
						pbRefreshing.setVisibility(View.GONE);
						layLoading.setVisibility(View.GONE);
					}
				}
				super.handleMessage(msg);
			}
		};
		final Timer tmr = new Timer();
		tmr.schedule(new TimerTask() {
			@Override
			public void run() {
				tmr.cancel();
				hTmr.sendEmptyMessage(1);
			}
		}, 1000);

		inProgress = true;
		pbRefreshing.setVisibility(View.VISIBLE);
		layLoading.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					inProgress = false;
					if (tmrEd) {
						pbRefreshing.setVisibility(View.GONE);
						layLoading.setVisibility(View.GONE);
					}
				}
				super.handleMessage(msg);
			}
		};

		tvArticle.post(new Runnable() {

			@Override
			public void run() {
				// String comment = GlobalInstance.currentArticle.getComment();
				tvArticle.loadUrl("http://gyue.cn/2012/07/11/github/");
				h.sendEmptyMessage(1);
			}
		});
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
//		case R.id.btnZoomIn:
//			fontSize++;
//			tvArticle.zoomIn();
//			PreferenceManager.getDefaultSharedPreferences(this).edit().putInt("font-size", fontSize).commit();
//			break;
//		case R.id.btnZoomOut:
//			fontSize--;
//			tvArticle.zoomOut();
//			PreferenceManager.getDefaultSharedPreferences(this).edit().putInt("font-size", fontSize).commit();
//			break;
		case R.id.tvSeeWeb:
			Intent inSeeWeb = new Intent(Intent.ACTION_VIEW);
			inSeeWeb.setData(Uri.parse(GlobalInstance.currentArticle.getLink()));
			startActivity(inSeeWeb);
			break;
		case R.id.btnBack:
			finish();
			break;
		case R.id.imgShareSina:
			if (GlobalInstance.sinaName.equals("")) {
				Toast.makeText(this, R.string.not_bind_sina, Toast.LENGTH_LONG).show();
				return;
			}
			Intent inSina = new Intent(this, ShareEditActivity.class);
			inSina.putExtra("to", "sina");
			startActivity(inSina);
			break;
		case R.id.imgShareTencent:
			if (GlobalInstance.tencentName.equals("")) {
				Toast.makeText(this, R.string.not_bind_tencent, Toast.LENGTH_LONG).show();
				return;
			}
			Intent inTencent = new Intent(this, ShareEditActivity.class);
			inTencent.putExtra("to", "tencent");
			startActivity(inTencent);
			break;
		}

	}

	public static List<String> getImages(String htmlStr) {
		Pattern p_image;
		Matcher m_image;
		List<String> pics = new ArrayList<String>();

		String regEx_img = "http://[([a-z0-9]|.|/|\\-)]+.[(jpg)|(bmp)|(gif)|(png)]";
		p_image = Pattern.compile(regEx_img, Pattern.CASE_INSENSITIVE);
		m_image = p_image.matcher(htmlStr);
		while (m_image.find()) {
			pics.add(m_image.group());
		}
		return pics;
	}

}
