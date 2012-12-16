package com.rarnu.zoe.love2;

import com.rarnu.zoe.love2.api.LovingYouApi;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.Button;
import android.widget.ImageView;

public class PhotoMethodActivity extends Activity implements OnClickListener {

	Button btnCamera, btnGallery;
	ImageView imgClose;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.activity_photo_method);

		btnCamera = (Button) findViewById(R.id.btnCamera);
		btnGallery = (Button) findViewById(R.id.btnGallery);
		imgClose = (ImageView) findViewById(R.id.imgClose);

		btnCamera.setOnClickListener(this);
		btnGallery.setOnClickListener(this);
		imgClose.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		int method = 0;
		switch (v.getId()) {
		case R.id.imgClose:
			LovingYouApi.saveLog(this, "PhotoMethodActivity", "Back");
			finish();
			return;
		case R.id.btnCamera:
			LovingYouApi.saveLog(this, "PhotoMethodActivity", "Camera");
			method = 0;
			break;
		case R.id.btnGallery:
			LovingYouApi.saveLog(this, "PhotoMethodActivity", "Gallery");
			method = 1;
			break;
		}

		Intent inRet = new Intent();
		inRet.putExtra("method", method);
		setResult(RESULT_OK, inRet);
		finish();

	}
}
