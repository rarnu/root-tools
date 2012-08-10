package com.rarnu.findaround;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ListView;

import com.rarnu.findaround.adapter.KeywordAdapter;
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.Config;

public class KeywordsActivity extends BaseActivity implements OnClickListener {

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
		init();

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

	@Override
	protected void init() {
		super.init();
		initEvents();
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();

		lvKeywords = (ListView) findViewById(R.id.lvKeywords);
	}

	private void initEvents() {
		backArea.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.backArea:
			finish();
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
