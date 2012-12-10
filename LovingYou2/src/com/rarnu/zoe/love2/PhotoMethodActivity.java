package com.rarnu.zoe.love2;

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
			finish();
			return;
		case R.id.btnCamera:
			method = 0;
			break;
		case R.id.btnGallery:
			method = 1;
			break;
		}

		Intent inRet = new Intent();
		inRet.putExtra("method", method);
		setResult(RESULT_OK, inRet);
		finish();

	}
}
