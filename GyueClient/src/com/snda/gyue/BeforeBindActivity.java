package com.snda.gyue;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

public class BeforeBindActivity extends Activity implements OnClickListener {

	Button btnStartAuth, btnBack;
	int authType = 0;
	
	@Override
	public void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);
		setContentView(R.layout.before_bind_weibo);
		
		authType = getIntent().getIntExtra("auth", 0);
		if (authType == 0) {
			Toast.makeText(this, R.string.error, Toast.LENGTH_LONG).show();
			finish();
			return;
		}
		
		btnStartAuth = (Button) findViewById(R.id.btnStartAuth);
		btnBack = (Button) findViewById(R.id.btnBack);
		btnStartAuth.setOnClickListener(this);
		btnBack.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnStartAuth:
			switch (authType) {
			case 1:
				Intent inSina = new Intent(this, BindSinaActivity.class);
				startActivity(inSina);
				finish();
				break;
			case 2:
				Intent inTencent = new Intent(this, BindTencentActivity.class);
				startActivity(inTencent);
				finish();
				break;
			}
			break;
		case R.id.btnBack:
			finish();
			break;
		}
		
	}
}
