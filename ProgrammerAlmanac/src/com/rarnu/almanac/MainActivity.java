package com.rarnu.almanac;

import java.util.List;

import android.app.AlertDialog;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.support.v4.app.FragmentActivity;
import android.support.v4.view.MenuItemCompat;
import android.text.Html;
import android.text.util.Linkify;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.view.Window;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.rarnu.almanac.Almanac.Result;

public class MainActivity extends FragmentActivity implements
		OnGlobalLayoutListener, OnClickListener {

	Almanac al;

	TextView tvToday;
	TextView tvDirection, tvDrink, tvGoddes;
	LinearLayout lvGood, lvBad;
	TextView tvGood, tvBad;
	ScrollView sv;
	TextView tvHelp;
	RelativeLayout layTitle;
	Button btnHelp;

	UpdateInfo updateInfo = null;

	private static final int MENUID_HELP = 1;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		if (android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.HONEYCOMB) {
			requestWindowFeature(Window.FEATURE_NO_TITLE);
		}
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(this, getWindowManager());
		setContentView(R.layout.activity_main);

		layTitle = (RelativeLayout) findViewById(R.id.layTitle);
		btnHelp = (Button) findViewById(R.id.btnHelp);

		if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.HONEYCOMB) {
			layTitle.setVisibility(View.GONE);
		}

		sv = (ScrollView) findViewById(R.id.sv);
		tvToday = (TextView) findViewById(R.id.tvToday);
		tvDirection = (TextView) findViewById(R.id.tvDirection);
		tvDrink = (TextView) findViewById(R.id.tvDrink);
		tvGoddes = (TextView) findViewById(R.id.tvGoddes);
		lvGood = (LinearLayout) findViewById(R.id.lvGood);
		lvBad = (LinearLayout) findViewById(R.id.lvBad);
		tvGood = (TextView) findViewById(R.id.tvGood);
		tvBad = (TextView) findViewById(R.id.tvBad);

		lvGood.getViewTreeObserver().addOnGlobalLayoutListener(this);
		lvBad.getViewTreeObserver().addOnGlobalLayoutListener(this);
		btnHelp.setOnClickListener(this);
		loadData();

		getUpdateInfo();

	}

	final Handler hUpdate = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				if (updateInfo != null && updateInfo.result != 0) {
					UpdateUtils.showUpdateInfo(MainActivity.this, updateInfo);
				}
			}
			super.handleMessage(msg);
		}
	};

	private void getUpdateInfo() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				int verCode = DeviceUtils.getAppVersionCode(MainActivity.this);
				updateInfo = MobileApi.checkUpdate(verCode);
				hUpdate.sendEmptyMessage(1);
			}
		}).start();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.HONEYCOMB) {
			MenuItem itemHelp = menu.add(0, MENUID_HELP, 1, R.string.help);
			MenuItemCompat.setShowAsAction(itemHelp,
					MenuItemCompat.SHOW_AS_ACTION_IF_ROOM);
			itemHelp.setIcon(android.R.drawable.ic_menu_help);
		}
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MENUID_HELP:
			showHelp();
			break;
		}
		return true;
	}

	private void loadData() {
		al = new Almanac();
		tvToday.setText(al.getTodayString());
		tvDirection.setText(Html.fromHtml(String.format(
				getString(R.string.direction_fmt), al.getDirectionString())));
		tvDrink.setText(Html.fromHtml(String.format(
				getString(R.string.drink_fmt), al.getDrinkString())));
		tvGoddes.setText(Html.fromHtml(String.format(
				getString(R.string.goddes_fmt), al.getGoddesString())));

		List<Result> listAll = al.pickTodaysLuck();
		for (Result r : listAll) {
			addLayout(r);
		}
	}

	private void addLayout(Result r) {
		View v = getLayoutInflater().inflate(R.layout.item, null);
		((TextView) v.findViewById(R.id.tvName)).setText(r.name);
		((TextView) v.findViewById(R.id.tvDesc)).setText(r.desc);
		(r.isGood ? lvGood : lvBad).addView(v);
	}

	@Override
	public void onGlobalLayout() {
		setTextHeight(tvGood, lvGood.getHeight());
		setTextHeight(tvBad, lvBad.getHeight());
	}

	private void setTextHeight(TextView tv, int height) {
		ViewGroup.LayoutParams vglp = tv.getLayoutParams();
		vglp.height = height;
		tv.setLayoutParams(vglp);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnHelp:
			showHelp();
			break;
		}
	}

	private void showHelp() {
		tvHelp = new TextView(this);
		tvHelp.setAutoLinkMask(Linkify.ALL);
		tvHelp.setTextColor(Color.WHITE);
		tvHelp.setTextSize(16);
		tvHelp.setLineSpacing(0F, 1.5F);
		tvHelp.setPadding(16, 16, 0, 0);
		tvHelp.setText(Html.fromHtml(getString(R.string.help_detail)));
		new AlertDialog.Builder(this).setTitle(R.string.help).setView(tvHelp)
				.setPositiveButton(R.string.close, null).show();
	}

}
