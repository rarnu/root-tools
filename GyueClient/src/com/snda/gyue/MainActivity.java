package com.snda.gyue;

import java.util.List;

import org.apache.http.protocol.HTTP;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.gyue.adapter.ArticleItemAdapter;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.network.HttpProxy;
import com.snda.gyue.utils.FileUtils;

public class MainActivity extends Activity implements OnClickListener {

	RelativeLayout btnFunc1, btnFunc2, btnFunc3, btnFunc4, btnFunc5;

	RelativeLayout layContent;

	ListView lvLatest, lvIndustry, lvApplication, lvGame, lvMyFavorate;

	List<ArticleItem> lstLatestArticles, lstIndustryArticles,
			lstApplicationArticles, lstGameArticles, lstMyFavorateArticles;

	ArticleItemAdapter adapterLatest, adapterIndustry, adapterApplication,
			adapterGame, adapterMyFavorate;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		requestWindowFeature(Window.FEATURE_NO_TITLE);
		requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);

		setContentView(R.layout.main);

		layContent = (RelativeLayout) findViewById(R.id.layContent);

		btnFunc1 = (RelativeLayout) findViewById(R.id.btnFunc1);
		btnFunc2 = (RelativeLayout) findViewById(R.id.btnFunc2);
		btnFunc3 = (RelativeLayout) findViewById(R.id.btnFunc3);
		btnFunc4 = (RelativeLayout) findViewById(R.id.btnFunc4);
		btnFunc5 = (RelativeLayout) findViewById(R.id.btnFunc5);

		setIconText(btnFunc1, R.drawable.information, R.string.func1);
		setIconText(btnFunc2, R.drawable.information, R.string.func2);
		setIconText(btnFunc3, R.drawable.information, R.string.func3);
		setIconText(btnFunc4, R.drawable.information, R.string.func4);
		setIconText(btnFunc5, R.drawable.information, R.string.func5);

		lvLatest = (ListView) getLayoutInflater().inflate(R.layout.page_latest,
				null);
		setContentLayout(lvLatest);

		adjustButtonWidth();

		getArticleListT(1, 1, 20);

	}

	private void getArticleListT(final int type, final int page,
			final int pageSize) {
		setProgressBarIndeterminateVisibility(true);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					switch (type) {
					case 1:
//						 lvLatest.setAdapter(adapterLatest);
						Toast.makeText(MainActivity.this, "ok", Toast.LENGTH_LONG).show();
						break;
					case 2:
						break;
					case 3:
						break;
					case 4:
						break;
					case 5:
						break;

					}
					setProgressBarIndeterminateVisibility(false);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				switch (type) {
				case 1:
					// TEST
//					lstLatestArticles = Test.getTestArticles();
//					if (lstLatestArticles != null) {
//						adapterLatest = new ArticleItemAdapter(
//								getLayoutInflater(), lstLatestArticles);
//					}
					
					try {
						String xml = HttpProxy.CallGet("http://www.gyue.cn/index.php", "m=content&c=rss&rssid=54&page=1&size=20", "GBK");
						FileUtils.rewriteFile("/sdcard/gyue.xml", xml);
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					
					break;
				case 2:
					// lstIndustryArticles
					break;
				case 3:
					// lstApplicationArticles
					break;
				case 4:
					// lstGameArticles
					break;
				case 5:
					// lstMyFavorateArticles
					break;
				}
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void switchPage(int page) {
		layContent.removeAllViews();
		switch (page) {
		case 1:
			layContent.addView(lvLatest);
			break;
		case 2:
			break;
		case 3:
			break;
		case 4:
			break;
		case 5:
			break;
		}
	}

	private void setContentLayout(View v) {
		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) v
				.getLayoutParams();
		if (lp == null) {
			lp = new RelativeLayout.LayoutParams(
					RelativeLayout.LayoutParams.MATCH_PARENT,
					RelativeLayout.LayoutParams.MATCH_PARENT);
		} else {
			lp.width = RelativeLayout.LayoutParams.MATCH_PARENT;
			lp.height = RelativeLayout.LayoutParams.MATCH_PARENT;
		}
		v.setLayoutParams(lp);
	}

	private void adjustButtonWidth() {
		DisplayMetrics metric = new DisplayMetrics();
		getWindowManager().getDefaultDisplay().getMetrics(metric);
		float density = metric.density;
		int wid = (getWindowManager().getDefaultDisplay().getWidth() - dipToPx(
				density, 40)) / 5;
		setButtonWidth(btnFunc1, wid);
		setButtonWidth(btnFunc2, wid);
		setButtonWidth(btnFunc3, wid);
		setButtonWidth(btnFunc4, wid);
		setButtonWidth(btnFunc5, wid);

	}

	private void setButtonWidth(RelativeLayout btn, int width) {
		LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) btn
				.getLayoutParams();
		lp.width = width;
		btn.setLayoutParams(lp);
	}

	public static int dipToPx(float density, int dip) {
		return (int) (dip * density + 0.5f);
	}

	private void setIconText(RelativeLayout btn, int icon, int text) {
		((ImageView) btn.findViewById(R.id.imgItemIco))
				.setBackgroundDrawable(getResources().getDrawable(icon));
		((TextView) btn.findViewById(R.id.tvItemName)).setText(text);
		btn.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		if (v instanceof RelativeLayout) {
			setSelectedItem((RelativeLayout) v);
		}
		switch (v.getId()) {
		case R.id.btnFunc1:
			switchPage(1);
			break;
		case R.id.btnFunc2:
			switchPage(2);
			break;
		case R.id.btnFunc3:
			switchPage(3);
			break;
		case R.id.btnFunc4:
			switchPage(4);
			break;
		case R.id.btnFunc5:
			switchPage(5);
			break;
		}
	}

	private void setSelectedItem(RelativeLayout btn) {
		btnFunc1.setBackgroundDrawable(null);
		btnFunc2.setBackgroundDrawable(null);
		btnFunc3.setBackgroundDrawable(null);
		btnFunc4.setBackgroundDrawable(null);
		btnFunc5.setBackgroundDrawable(null);
		btn.setBackgroundDrawable(getResources().getDrawable(
				R.drawable.item_focus));
	}

}