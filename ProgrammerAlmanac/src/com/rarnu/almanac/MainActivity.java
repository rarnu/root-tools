package com.rarnu.almanac;

import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.text.Html;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.rarnu.almanac.Almanac.Result;

public class MainActivity extends Activity implements OnGlobalLayoutListener {

	Almanac al;


	TextView tvToday;
	TextView tvDirection, tvDrink, tvGoddes;
	LinearLayout lvGood, lvBad;
	TextView tvGood, tvBad;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(this, getWindowManager());
		setContentView(R.layout.activity_main);
		
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
		((TextView)v.findViewById(R.id.tvName)).setText(r.name);
		((TextView)v.findViewById(R.id.tvDesc)).setText(r.desc);
		(r.isGood?lvGood:lvBad).addView(v);
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

}
