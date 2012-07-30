package com.rarnu.findaround;

import com.rarnu.findaround.common.Config;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

public class KeywordsActivity extends Activity implements OnClickListener {

	TextView tvName;
	Button btnLeft;
	EditText etKeywords;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.keywords);
		mappingComponents();
		initEvents();
		etKeywords.setText(Config.getKeywordsText(this));
	}
	
	private void mappingComponents() {
		tvName = (TextView) findViewById(R.id.tvName);
		tvName.setText(R.string.settings_keywords);
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnLeft.setVisibility(View.VISIBLE);
		btnLeft.setText(R.string.save);
		etKeywords = (EditText) findViewById(R.id.etKeywords);
	}
	
	private void initEvents() {
		btnLeft.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			Config.setKeywords(this, etKeywords.getText().toString());
			finish();
			break;
		}
	}
}
