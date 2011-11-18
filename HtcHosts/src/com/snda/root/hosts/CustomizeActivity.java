package com.snda.root.hosts;

import android.app.Activity;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class CustomizeActivity extends Activity implements OnClickListener {

	EditText etHosts;
	Button btnSave, btnCancel;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.customize);

		etHosts = (EditText) findViewById(R.id.etHosts);
		btnSave = (Button) findViewById(R.id.btnSave);
		btnCancel = (Button) findViewById(R.id.btnCancel);

		btnSave.setOnClickListener(this);
		btnCancel.setOnClickListener(this);

		etHosts.setText(MiscUtils.readCustomizeHosts(this));
	}

	@Override
	public void onClick(View view) {
		switch (view.getId()) {
		case R.id.btnSave:
			MiscUtils.writeCustomizeHosts(this, etHosts.getText().toString());
			finish();
			break;
		case R.id.btnCancel:
			finish();
			break;
		}

	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

}
