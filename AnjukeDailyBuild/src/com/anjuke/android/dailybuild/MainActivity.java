package com.anjuke.android.dailybuild;

import java.util.List;

import com.anjuke.android.dailybuild.api.MobileAPI;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

@SuppressLint("HandlerLeak")
public class MainActivity extends Activity {

	ListView lvDB;
	TextView tvLoading;
	List<String> list = null;
	ArrayAdapter<String> adapter = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		lvDB = (ListView) findViewById(R.id.lvDB);
		tvLoading = (TextView) findViewById(R.id.tvLoading);

		loadProjectListT();

	}

	private void loadProjectListT() {
		tvLoading.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {
			@Override
			public void handleMessage(android.os.Message msg) {
				if (msg.what == 1) {
					tvLoading.setVisibility(View.GONE);
					if (list == null) {
						adapter = null;
					} else {
						adapter = new ArrayAdapter<String>(MainActivity.this,
								R.layout.list_item, R.id.tvListItem, list);
					}
					lvDB.setAdapter(adapter);
				}
				super.handleMessage(msg);
			};

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				list = MobileAPI.getProjectList();
				h.sendEmptyMessage(1);

			}
		}).start();
	}

}
