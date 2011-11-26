package com.snda.root.hosts;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

public class AddHostActivity extends Activity implements OnClickListener {
	
	EditText etAddIP, etAddDomain;
	Button btnAddOK, btnAddCancel;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		
		setContentView(R.layout.add_host);
		etAddIP = (EditText) findViewById(R.id.etAddIP);
		etAddDomain = (EditText) findViewById(R.id.etAddDomain);
		btnAddOK = (Button) findViewById(R.id.btnAddOK);
		btnAddCancel = (Button) findViewById(R.id.btnAddCancel);
		
		btnAddOK.setOnClickListener(this);
		btnAddCancel.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnAddOK:
			String ip = etAddIP.getText().toString();
			String domain = etAddDomain.getText().toString();
			if (ip.equals("") || domain.equals("")) {
				Toast.makeText(this, R.string.c_inputinfo, Toast.LENGTH_LONG).show();
				break;
			}
			Intent inResult = new Intent();
			inResult.putExtra("IP", ip);
			inResult.putExtra("DOMAIN", domain);
			setResult(RESULT_OK, inResult);
			finish();
			break;
		case R.id.btnAddCancel:
			finish();
			break;
		}
		
	}

}
