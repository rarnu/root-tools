package com.snda.gyue;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.Gallery;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.snda.gyue.adapter.ArticleItemAdapter;
import com.snda.gyue.adapter.ImageAdapter;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.network.HttpProxy;
import com.snda.gyue.network.ItemBuilder;
import com.snda.gyue.utils.ImageUtils;

public class MainActivity extends Activity implements OnClickListener, OnItemClickListener {

	RelativeLayout btnFunc1, btnFunc2, btnFunc3, btnFunc4, btnFunc5;

	ScrollView layContent;
	ListView lvArticles;
	List<ArticleItem> lstArticles;
	ArticleItemAdapter adapter;
	ProgressBar pbRefreshing;
	Button btnRefresh, btnBack;
	Gallery gallaryPhotos;

	int CurrentType = 0;
	boolean inProgress = false;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		requestWindowFeature(Window.FEATURE_NO_TITLE);

		getWindowManager().getDefaultDisplay().getMetrics(GlobalInstance.metric);
		GlobalInstance.density = GlobalInstance.metric.density;

		setContentView(R.layout.main);

		layContent = (ScrollView) findViewById(R.id.layContent);

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

		lvArticles = (ListView) findViewById(R.id.lvArticles);
		pbRefreshing = (ProgressBar) findViewById(R.id.pbRefreshing);
		btnRefresh = (Button) findViewById(R.id.btnRefresh);
		btnBack = (Button) findViewById(R.id.btnBack);
		gallaryPhotos = (Gallery) findViewById(R.id.gallaryPhotos);

		btnRefresh.setOnClickListener(this);
		btnBack.setOnClickListener(this);
		lvArticles.setOnItemClickListener(this);

		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) gallaryPhotos.getLayoutParams();
		lp.height = (int) (260 * GlobalInstance.metric.widthPixels / 480);
		gallaryPhotos.setLayoutParams(lp);

		gallaryPhotos.setOnItemClickListener(this);

		adjustButtonWidth();

		onClick(btnFunc1);

	}

	private void getArticleListT(final int type, final int page) {

		inProgress = true;
		btnRefresh.setEnabled(false);
		pbRefreshing.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					lvArticles.setAdapter(adapter);

					RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) lvArticles.getLayoutParams();
					lp.height = ImageUtils.dipToPx(GlobalInstance.density, 81) * lstArticles.size();
					lvArticles.setLayoutParams(lp);
					btnRefresh.setEnabled(true);
					pbRefreshing.setVisibility(View.GONE);
					if (type == 54) {
						setGalleryImages(lstArticles);
					}
					inProgress = false;
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				try {
					String xml = HttpProxy.CallGet(GyueConsts.SITE_URL, String.format(GyueConsts.REQ_PARAMS, type, page, GyueConsts.PAGE_SIZE), "GBK");
					lstArticles = ItemBuilder.xmlToItems(MainActivity.this, type, xml, false);
					if (lstArticles != null) {
						adapter = new ArticleItemAdapter(getLayoutInflater(), lstArticles);
					} else {
						adapter = null;
					}
				} catch (Exception e) {

				}
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void adjustButtonWidth() {

		int wid = (getWindowManager().getDefaultDisplay().getWidth() - ImageUtils.dipToPx(GlobalInstance.density, 40)) / 5;
		setButtonWidth(btnFunc1, wid);
		setButtonWidth(btnFunc2, wid);
		setButtonWidth(btnFunc3, wid);
		setButtonWidth(btnFunc4, wid);
		setButtonWidth(btnFunc5, wid);

	}

	private void setButtonWidth(RelativeLayout btn, int width) {
		LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) btn.getLayoutParams();
		lp.width = width;
		btn.setLayoutParams(lp);
	}

	public void setGalleryImages(List<ArticleItem> images) {
		List<ArticleItem> list = new ArrayList<ArticleItem>();
		for (int i = 0; i < images.size(); i++) {
			if ((images.get(i).getArticleImageLocalFileName() != null) && (!images.get(i).getArticleImageLocalFileName().equals(""))) {
				list.add(images.get(i));
				if (list.size() >= 5) {
					break;
				}
			}
		}
		ImageAdapter imgAdapter = new ImageAdapter(this, list);
		gallaryPhotos.setAdapter(imgAdapter);
	}

	private void setIconText(RelativeLayout btn, int icon, int text) {
		((ImageView) btn.findViewById(R.id.imgItemIco)).setBackgroundDrawable(getResources().getDrawable(icon));
		((TextView) btn.findViewById(R.id.tvItemName)).setText(text);
		btn.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {

		if (inProgress) {
			return;
		}

		if (v instanceof Button) {
			switch (v.getId()) {
			case R.id.btnBack:
				finish();
				break;
			case R.id.btnRefresh:
				getArticleListT(CurrentType, 1);
				break;
			}
			return;
		}

		if (v instanceof RelativeLayout) {
			setSelectedItem((RelativeLayout) v);
		}

		gallaryPhotos.setVisibility(View.GONE);

		switch (v.getId()) {
		case R.id.btnFunc1:
			CurrentType = 54;
			gallaryPhotos.setVisibility(View.VISIBLE);
			break;
		case R.id.btnFunc2:
			CurrentType = 13;
			break;
		case R.id.btnFunc3:
			CurrentType = 11;
			break;
		case R.id.btnFunc4:
			CurrentType = 12;
			break;
		case R.id.btnFunc5:

			return;
		}
		getArticleListT(CurrentType, 1);
	}

	private void setSelectedItem(RelativeLayout btn) {
		btnFunc1.setBackgroundDrawable(null);
		btnFunc2.setBackgroundDrawable(null);
		btnFunc3.setBackgroundDrawable(null);
		btnFunc4.setBackgroundDrawable(null);
		btnFunc5.setBackgroundDrawable(null);
		btn.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

		ArticleItem item = null;
		switch (parent.getId()) {
		case R.id.lvArticles:
			item = (ArticleItem) lvArticles.getItemAtPosition(position);
			break;
		case R.id.gallaryPhotos:
			item = (ArticleItem) gallaryPhotos.getItemAtPosition(position);
			break;
		}

		if (item != null) {
			GlobalInstance.currentArticle = item;
			Intent inArticle = new Intent(this, ViewArticleActivity.class);
			startActivity(inArticle);
		}

	}

}