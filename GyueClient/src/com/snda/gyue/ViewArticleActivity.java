package com.snda.gyue;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
import android.text.Html.ImageGetter;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.snda.gyue.network.NetFiles;
import com.snda.gyue.utils.ImageUtils;

public class ViewArticleActivity extends Activity implements OnClickListener {

	Button btnBack;
	ProgressBar pbRefreshing;
	TextView tvArticle;
	TextView tvTitle, tvDate;
	ScrollView layContent;
	TextView tvSeeWeb;
	RelativeLayout layLoading;
	ImageGetter iGetter;
	Handler hPack;
	ImageView imgShareTencent, imgShareSina;

	boolean inProgress = false;
	boolean tmrEd = false;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.view_article);

		btnBack = (Button) findViewById(R.id.btnBack);
		pbRefreshing = (ProgressBar) findViewById(R.id.pbRefreshing);
		tvArticle = (TextView) findViewById(R.id.tvArticle);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
		tvDate = (TextView) findViewById(R.id.tvDate);
		layContent = (ScrollView) findViewById(R.id.layContent);
		tvSeeWeb = (TextView) findViewById(R.id.tvSeeWeb);
		layLoading = (RelativeLayout) findViewById(R.id.layLoading);
		imgShareTencent = (ImageView) findViewById(R.id.imgShareTencent);
		imgShareSina = (ImageView) findViewById(R.id.imgShareSina);

		btnBack.setOnClickListener(this);
		tvSeeWeb.setOnClickListener(this);
		imgShareSina.setOnClickListener(this);
		imgShareTencent.setOnClickListener(this);

		tvTitle.setText(GlobalInstance.currentArticle.getTitle());
		tvDate.setText(GlobalInstance.currentArticle.getDate());

		hPack = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 99) {
					int y = layContent.getScrollY();
					setTextView();
					layContent.scrollTo(0, y);
				}
				super.handleMessage(msg);
			}
		};

		if (!getIntent().getBooleanExtra("no_pic", false)) {
			NetFiles.doDownloadImagePackT(this, getImages(GlobalInstance.currentArticle.getComment()), hPack);
		}

		iGetter = new ImageGetter() {

			@Override
			public Drawable getDrawable(String source) {
				Drawable drawable = null;

				if (!getIntent().getBooleanExtra("no_pic", false)) {

					String local = NetFiles.buildLocalFileName(source);
					File fImg = new File(local);
					if (!fImg.exists()) {
						return new BitmapDrawable(getResources(), BitmapFactory.decodeResource(getResources(),
								R.drawable.empty));
					}
					Bitmap bmp = ImageUtils.doMatrix(BitmapFactory.decodeFile(local), 0,
							GlobalInstance.metric.widthPixels, GlobalInstance.metric.heightPixels);
					drawable = new BitmapDrawable(getResources(), bmp);
					drawable.setBounds(0, 0, drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight());
					return drawable;
				}
				return new BitmapDrawable(getResources(),
						BitmapFactory.decodeResource(getResources(), R.drawable.empty));
			}
		};

		setTextView();
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
				String comment = GlobalInstance.currentArticle.getComment();
				tvArticle.setText(Html.fromHtml(comment, iGetter, null));
				h.sendEmptyMessage(1);
			}
		});
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvSeeWeb:
			Intent inSeeWeb = new Intent(Intent.ACTION_VIEW);
			inSeeWeb.setData(Uri.parse(GlobalInstance.currentArticle.getLink()));
			startActivity(inSeeWeb);
			break;
		case R.id.btnBack:
			finish();
			break;
		case R.id.imgShareSina:
			// TODO: share to sina
			break;
		case R.id.imgShareTencent:
			// TODO: share to tencent
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
