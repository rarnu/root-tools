package com.anjuke.android.dailybuild;

import java.util.List;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.anjuke.android.dailybuild.api.MobileAPI;

@SuppressLint("HandlerLeak")
public class MainActivity extends Activity implements OnItemClickListener {

	ListView lvDB;
	TextView tvLoading;
	List<String> list = null;
	ArrayAdapter<String> adapter = null;
	ImageView imgNoRecord;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		lvDB = (ListView) findViewById(R.id.lvDB);
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		imgNoRecord = (ImageView) findViewById(R.id.imgNoRecord);

		lvDB.setOnItemClickListener(this);
		loadProjectListT();

	}

	private void loadProjectListT() {
		imgNoRecord.setVisibility(View.GONE);
		tvLoading.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {
			@Override
			public void handleMessage(android.os.Message msg) {
				if (msg.what == 1) {
					tvLoading.setVisibility(View.GONE);
					if (list == null) {
						adapter = null;
						imgNoRecord.setVisibility(View.VISIBLE);
					} else {
						adapter = new ArrayAdapter<String>(MainActivity.this,
								R.layout.list_item, R.id.tvListItem, list);
						imgNoRecord.setVisibility(View.GONE);
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

	@Override
	public void onItemClick(AdapterView<?> adapter, View v, int position,
			long id) {
		String folder = (String) lvDB.getItemAtPosition(position);
		
		Intent inBuild = new Intent(this, BuildActivity.class);
		inBuild.putExtra("folder", folder);
		startActivity(inBuild);
		
		
	}

}
