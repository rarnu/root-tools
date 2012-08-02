package com.rarnu.findaround;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.findaround.adapter.KeywordAdapter;
import com.rarnu.findaround.common.Config;
import com.rarnu.findaround.comp.AlertDialogEx;
import com.rarnu.findaround.comp.AlertDialogEx.DialogButtonClickListener;

public class KeywordsActivity extends Activity implements OnClickListener {

	TextView tvName;
	Button btnLeft, btnRight;
	ListView lvKeywords;
	KeywordAdapter adapter;
	List<String> list;

	Handler h = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				list.remove(msg.arg1);
				saveKeywordList();
				adapter.notifyDataSetChanged();
			}
			super.handleMessage(msg);
		};

	};

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.keywords);
		mappingComponents();
		initEvents();

		String keywords = Config.getKeywordsText(this);
		String[] arrKeywords = keywords.split("\n");
		list = new ArrayList<String>();
		for (int i = 0; i < arrKeywords.length; i++) {
			if (!arrKeywords[i].trim().equals("")) {
				list.add(arrKeywords[i]);
			}
		}

		adapter = new KeywordAdapter(getLayoutInflater(), list, h);
		lvKeywords.setAdapter(adapter);
		// etKeywords.setText(Config.getKeywordsText(this));
	}

	private void mappingComponents() {
		tvName = (TextView) findViewById(R.id.tvName);
		tvName.setText(R.string.settings_keywords);
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnLeft.setVisibility(View.VISIBLE);
		btnLeft.setText(R.string.back);
		btnRight = (Button) findViewById(R.id.btnRight);
		btnRight.setVisibility(View.VISIBLE);
		btnRight.setText(R.string.add);
		lvKeywords = (ListView) findViewById(R.id.lvKeywords);
		// etKeywords = (EditText) findViewById(R.id.etKeywords);
	}

	private void initEvents() {
		btnLeft.setOnClickListener(this);
		btnRight.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:

			AlertDialogEx.showAlertDialogEx(this,
					getString(R.string.add_keyword),
					getString(R.string.add_keyword_hint),
					getString(R.string.ok), new DialogButtonClickListener() {

						@Override
						public void onClick(View v, String text) {
							list.add(text);
							adapter.notifyDataSetChanged();
							saveKeywordList();

						}
					}, getString(R.string.cancel), null);
			break;
		}
	}

	private void saveKeywordList() {
		String key = "";
		for (int i = 0; i < list.size(); i++) {
			key += list.get(i) + "\n";
		}
		Config.setKeywords(this, key);
	}
}
