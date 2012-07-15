package com.rarnu.findaround;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class SettingsActivity extends Activity implements OnClickListener {

	Button btnLeft;
	TextView tvName;

	RelativeLayout layDist1, layDist2, layDist3, layMethod1, layMethod2,
			layKeywords;
	ImageView imgDist1, imgDist2, imgDist3, imgMethod1, imgMethod2;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.settings);
		mappingComponents();
		initEvents();
		initSettings();
	}

	private void mappingComponents() {
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnLeft.setVisibility(View.VISIBLE);
		btnLeft.setText(R.string.back);
		tvName = (TextView) findViewById(R.id.tvName);
		tvName.setText(R.string.settings);
		layDist1 = (RelativeLayout) findViewById(R.id.layDist1);
		layDist2 = (RelativeLayout) findViewById(R.id.layDist2);
		layDist3 = (RelativeLayout) findViewById(R.id.layDist3);
		layMethod1 = (RelativeLayout) findViewById(R.id.layMethod1);
		layMethod2 = (RelativeLayout) findViewById(R.id.layMethod2);
		imgDist1 = (ImageView) findViewById(R.id.imgDist1);
		imgDist2 = (ImageView) findViewById(R.id.imgDist2);
		imgDist3 = (ImageView) findViewById(R.id.imgDist3);
		imgMethod1 = (ImageView) findViewById(R.id.imgMethod1);
		imgMethod2 = (ImageView) findViewById(R.id.imgMethod2);
		layKeywords = (RelativeLayout) findViewById(R.id.layKeywords);
	}

	private void initEvents() {
		btnLeft.setOnClickListener(this);
		layDist1.setOnClickListener(this);
		layDist2.setOnClickListener(this);
		layDist3.setOnClickListener(this);
		layMethod1.setOnClickListener(this);
		layMethod2.setOnClickListener(this);
		layKeywords.setOnClickListener(this);
	}

	private void initSettings() {
		int dist = Config.getDist(this);
		imgDist1.setBackgroundDrawable(null);
		imgDist2.setBackgroundDrawable(null);
		imgDist3.setBackgroundDrawable(null);
		switch (dist) {
		case 1000:
			imgDist1.setBackgroundResource(R.drawable.checked);
			break;
		case 3000:
			imgDist2.setBackgroundResource(R.drawable.checked);
			break;
		case 5000:
			imgDist3.setBackgroundResource(R.drawable.checked);
			break;
		}
		int method = Config.getMethod(this);
		imgMethod1.setBackgroundDrawable(null);
		imgMethod2.setBackgroundDrawable(null);
		switch (method) {
		case 1:
			imgMethod1.setBackgroundResource(R.drawable.checked);
			break;
		case 2:
			imgMethod2.setBackgroundResource(R.drawable.checked);
			break;
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.layDist1:
			Config.setDist(this, 1000);
			break;
		case R.id.layDist2:
			Config.setDist(this, 3000);
			break;
		case R.id.layDist3:
			Config.setDist(this, 5000);
			break;
		case R.id.layMethod1:
			Config.setMethod(this, 1);
			break;
		case R.id.layMethod2:
			Config.setMethod(this, 2);
			break;
		case R.id.layKeywords:
			Intent inKeywords = new Intent(this, KeywordsActivity.class);
			startActivity(inKeywords);
			break;
		}
		initSettings();
	}
}
