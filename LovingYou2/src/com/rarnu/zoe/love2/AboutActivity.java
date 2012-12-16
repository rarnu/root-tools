package com.rarnu.zoe.love2;

import java.io.IOException;

import com.rarnu.zoe.love2.utils.FileUtils;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class AboutActivity extends Activity {

	TextView tvAbout, tvDisclaimer;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_about);
		
		tvAbout = (TextView) findViewById(R.id.tvAbout);
		tvDisclaimer = (TextView) findViewById(R.id.tvDisclaimer);
		try {
			tvAbout.setText(FileUtils.readAssetFile(this, "about"));
			tvDisclaimer.setText(FileUtils.readAssetFile(this, "disclaimer"));
		} catch (IOException e) {

		}
	}
}
