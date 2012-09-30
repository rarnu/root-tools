package com.rarnu.onexcore;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

public class MainActivity extends Activity implements OnClickListener {

	Button[] btnCore;
	ImageView[] imgCore;
	TextView tvAbout;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		btnCore = new Button[4];
		btnCore[0] = (Button) findViewById(R.id.btnCore1);
		btnCore[1] = (Button) findViewById(R.id.btnCore2);
		btnCore[2] = (Button) findViewById(R.id.btnCore3);
		btnCore[3] = (Button) findViewById(R.id.btnCore4);
		imgCore = new ImageView[4];
		imgCore[0] = (ImageView) findViewById(R.id.imgCore1);
		imgCore[1] = (ImageView) findViewById(R.id.imgCore2);
		imgCore[2] = (ImageView) findViewById(R.id.imgCore3);
		imgCore[3] = (ImageView) findViewById(R.id.imgCore4);
		tvAbout = (TextView) findViewById(R.id.tvAbout);
		tvAbout.setOnClickListener(this);

		for (int i = 0; i < btnCore.length; i++) {
			btnCore[i].setOnClickListener(this);
		}

		load();
	}

	private void load() {
		int core = 0;
		try {
			core = OneXCoreUtils.getCoreCount();
		} catch (Exception e) {
			core = 0;
		}
		for (int i = 0; i < btnCore.length; i++) {
			imgCore[i].setBackgroundResource(R.drawable.button_grey);
		}
		if (core != 0) {
			imgCore[core - 1].setBackgroundResource(R.drawable.button_green);
		} else {
			for (int i = 0; i < btnCore.length; i++) {
				btnCore[i].setEnabled(false);
			}
		}
	}

	@Override
	public void onClick(View v) {
		int core = 0;
		switch (v.getId()) {
		case R.id.btnCore1:
			core = 1;
			break;
		case R.id.btnCore2:
			core = 2;
			break;
		case R.id.btnCore3:
			core = 3;
			break;
		case R.id.btnCore4:
			core = 4;
			break;
		case R.id.tvAbout:
			Intent inWeb = new Intent(Intent.ACTION_VIEW);
			inWeb.setData(Uri.parse(getString(R.string.site)));
			startActivity(inWeb);
			return;
		}

		if (core == 0) {
			core = 4;
		}

		OneXCoreUtils.setCoreCount(core);
		load();
	}

}
