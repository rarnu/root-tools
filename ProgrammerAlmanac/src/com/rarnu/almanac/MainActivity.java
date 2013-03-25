package com.rarnu.almanac;

import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.graphics.Color;
import android.os.Bundle;
import android.text.Html;
import android.text.util.Linkify;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.rarnu.almanac.Almanac.Result;

public class MainActivity extends Activity implements OnGlobalLayoutListener {

	Almanac al;

	TextView tvToday;
	TextView tvDirection, tvDrink, tvGoddes;
	LinearLayout lvGood, lvBad;
	TextView tvGood, tvBad;
	ScrollView sv;
	TextView tvHelp;

//	private static final int MENUID_SHARE = 0;
	private static final int MENUID_HELP = 1;

	// private String fn = Environment.getExternalStorageDirectory()
	// .getAbsolutePath() + "/screenshot.png";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(this, getWindowManager());
		setContentView(R.layout.activity_main);

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
		loadData();

		

	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// MenuItem itemShare = menu.add(0, MENUID_SHARE, 0, R.string.share);
		MenuItem itemHelp = menu.add(0, MENUID_HELP, 1, R.string.help);

		// itemShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
		itemHelp.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		// itemShare.setIcon(android.R.drawable.ic_menu_share);
		itemHelp.setIcon(android.R.drawable.ic_menu_help);

		// ShareActionProvider actionProvider = new ShareActionProvider(this);
		// itemShare.setActionProvider(actionProvider);
		// actionProvider
		// .setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME);
		// actionProvider.setShareIntent(createShareIntent());

		return true;
	}

	// private Intent createShareIntent() {
	//
	// File fBmp = new File(fn);
	// Intent shareIntent = new Intent(Intent.ACTION_SEND);
	// shareIntent.setType("image/*");
	// Uri uri = Uri.fromFile(fBmp);
	// shareIntent.putExtra(Intent.EXTRA_STREAM, uri);
	// shareIntent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_body));
	// shareIntent.putExtra(Intent.EXTRA_SUBJECT,
	// getString(R.string.share_title));
	// return shareIntent;
	// }

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MENUID_HELP:

			tvHelp = new TextView(this);
			tvHelp.setAutoLinkMask(Linkify.ALL);
			tvHelp.setTextColor(Color.WHITE);
			tvHelp.setTextSize(16);
			tvHelp.setLineSpacing(0F, 1.5F);
			tvHelp.setPadding(16, 16, 0, 0);
			tvHelp.setText(Html.fromHtml(getString(R.string.help_detail)));
			new AlertDialog.Builder(this).setTitle(R.string.help)
					.setView(tvHelp).setPositiveButton(R.string.close, null)
					.show();
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

//	private void screenshot() {
//		String fn = Environment.getExternalStorageDirectory().getAbsolutePath()
//				+ "/screenshot.png";
//		ImageUtils.takeScreenShot(sv, fn);
//	}
}
