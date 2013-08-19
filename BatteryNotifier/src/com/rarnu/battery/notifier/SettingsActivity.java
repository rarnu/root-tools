package com.rarnu.battery.notifier;

import com.rarnu.battery.notifier.common.Config;

import android.app.Activity;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.widget.EditText;

public class SettingsActivity extends Activity {

	EditText etCanCharge;
	EditText etCanFastCharge;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_settings);
		etCanCharge = (EditText) findViewById(R.id.etCanCharge);
		etCanFastCharge = (EditText) findViewById(R.id.etCanFastCharge);

		etCanCharge.setText(String.valueOf(Config.getCanCharge(this)));
		etCanFastCharge.setText(String.valueOf(Config.getFastCharge(this)));

		etCanCharge.addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {
			}

			@Override
			public void afterTextChanged(Editable s) {
				int p = Integer.parseInt(etCanCharge.getText().toString());
				Config.setCanCharge(SettingsActivity.this, p);
			}
		});

		etCanFastCharge.addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {

			}

			@Override
			public void afterTextChanged(Editable s) {
				int p = Integer.parseInt(etCanFastCharge.getText().toString());
				Config.setFastCharge(SettingsActivity.this, p);
			}
		});
	}
}
