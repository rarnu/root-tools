package com.rarnu.almanac;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import android.app.AlertDialog;
import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
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
import android.widget.TextView;

import com.rarnu.almanac.Almanac.Result;
import com.rarnu.almanac.api.MobileApi;
import com.rarnu.almanac.api.UpdateInfo;
import com.rarnu.almanac.utils.DeviceUtils;
import com.rarnu.almanac.utils.ImageUtils;
import com.rarnu.almanac.utils.UIUtils;
import com.rarnu.almanac.utils.UpdateUtils;

public class MainActivity extends FragmentActivity implements
		OnGlobalLayoutListener, OnClickListener {

	Almanac al;

	TextView tvToday;
	TextView tvDirection, tvDrink, tvGoddes;
	LinearLayout lvGood, lvBad;
	TextView tvGood, tvBad;
	RelativeLayout layAlmanac;
	TextView tvHelp;
	RelativeLayout layTitle;
	Button btnHelp, btnShare;
	RelativeLayout layMain;

	UpdateInfo updateInfo = null;

	private static final int MENUID_SHARE = 0;
	private static final int MENUID_HELP = 1;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		if (android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.HONEYCOMB) {
			requestWindowFeature(Window.FEATURE_NO_TITLE);
		}
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(this, getWindowManager());
		setContentView(R.layout.activity_main);

		layMain = (RelativeLayout) findViewById(R.id.layMain);
		layTitle = (RelativeLayout) findViewById(R.id.layTitle);
		btnHelp = (Button) findViewById(R.id.btnHelp);
		btnShare = (Button) findViewById(R.id.btnShare);

		if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.HONEYCOMB) {
			layTitle.setVisibility(View.GONE);
		} else {
			layMain.setBackgroundColor(Color.WHITE);
		}

		layAlmanac = (RelativeLayout) findViewById(R.id.layAlmanac);
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
		btnShare.setOnClickListener(this);
		loadData();
		getUpdateInfo();
	}

	private Handler hShare = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				String fn = (String) msg.obj;
				shareTo(fn);
			}
			super.handleMessage(msg);
		};
	};

	private void shareTo(String fn) {
		Intent intent = new Intent(Intent.ACTION_SEND);
		intent.setType("image/*");
		intent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.share_title));
		intent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_body));
		intent.putExtra(Intent.EXTRA_STREAM, Uri.parse(fn));
		intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		startActivity(Intent
				.createChooser(intent, getString(R.string.share_to)));
	}

	private void saveTodaysAlmanacT() {
		String fn = Environment.getExternalStorageDirectory().getAbsolutePath()
				+ "/.almanac/";
		File fTmpDir = new File(fn);
		if (!fTmpDir.exists()) {
			fTmpDir.mkdirs();
		}
		fn += new SimpleDateFormat("yyyyMMdd").format(new Date()) + ".png";
		final String finalFn = fn;

		final Message msg = new Message();
		msg.what = 1;
		msg.obj = finalFn;

		File fScreenshot = new File(finalFn);
		if (fScreenshot.exists()) {
			hShare.sendMessage(msg);
			return;
		}

		new Thread(new Runnable() {
			@Override
			public void run() {
				ImageUtils.takeScreenShot(layAlmanac, finalFn);
				hShare.sendMessage(msg);
			}
		}).start();
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

			MenuItem itemShare = menu.add(0, MENUID_SHARE, 0, R.string.share);
			MenuItemCompat.setShowAsAction(itemShare,
					MenuItemCompat.SHOW_AS_ACTION_IF_ROOM);
			itemShare.setIcon(android.R.drawable.ic_menu_share);

			MenuItem itemHelp = menu.add(0, MENUID_HELP, 1, R.string.help);
			MenuItemCompat.setShowAsAction(itemHelp,
					MenuItemCompat.SHOW_AS_ACTION_IF_ROOM);
			itemHelp.setIcon(android.R.drawable.ic_menu_info_details);
		}
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MENUID_SHARE:
			saveTodaysAlmanacT();
			break;
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

		int width = UIUtils.getWidth() - UIUtils.dipToPx(32);
		width /= 3;
		resizeView(tvDirection, width);
		resizeView(tvGoddes, width);
	}

	private void resizeView(View v, int width) {
		ViewGroup.LayoutParams vglp = v.getLayoutParams();
		vglp.width = width;
		v.setLayoutParams(vglp);
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
		case R.id.btnShare:
			saveTodaysAlmanacT();
			break;
		case R.id.btnHelp:
			showHelp();
			break;
		}
	}

	private void showHelp() {
		tvHelp = new TextView(this);
		tvHelp.setAutoLinkMask(Linkify.ALL);
		if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.HONEYCOMB) {
			tvHelp.setTextColor(Color.BLACK);
		} else {
			tvHelp.setTextColor(Color.WHITE);
		}
		tvHelp.setTextSize(16);
		tvHelp.setLineSpacing(0F, 1.5F);
		tvHelp.setPadding(16, 16, 0, 0);
		tvHelp.setText(Html.fromHtml(getString(R.string.help_detail)));
		new AlertDialog.Builder(this).setTitle(R.string.help).setView(tvHelp)
				.setPositiveButton(R.string.close, null).show();
	}

}
