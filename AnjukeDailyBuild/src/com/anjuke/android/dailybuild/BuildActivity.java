package com.anjuke.android.dailybuild;

import java.util.List;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.anjuke.android.dailybuild.api.MobileAPI;

@SuppressLint("HandlerLeak")
public class BuildActivity extends Activity implements OnItemClickListener {

	TextView tvTitle;
	TextView tvLoading;
	ListView lvFiles;
	EditText etFilter;
	List<String> list = null;
	BuildAdapter<String> adapter = null;
	ImageView imgNoRecord;
	String folder = "";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.build);

		lvFiles = (ListView) findViewById(R.id.lvFiles);
		tvLoading = (TextView) findViewById(R.id.tvLoading);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
		imgNoRecord = (ImageView) findViewById(R.id.imgNoRecord);
		etFilter = (EditText) findViewById(R.id.etFilter);
		lvFiles.setOnItemClickListener(this);

		etFilter.addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {
				// TODO Auto-generated method stub

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {
				// TODO Auto-generated method stub

			}

			@Override
			public void afterTextChanged(Editable s) {
				if (s == null || adapter == null) {
					return;
				}
				adapter.getFilter().filter(etFilter.getText().toString());

			}
		});

		folder = getIntent().getStringExtra("folder");
		tvTitle.setText(String.format(getString(R.string.build), folder));
		getFileListT(folder);

	}

	private void getFileListT(final String folder) {
		imgNoRecord.setVisibility(View.GONE);
		etFilter.setVisibility(View.GONE);
		tvLoading.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tvLoading.setVisibility(View.GONE);
					if (list == null) {
						adapter = null;
						imgNoRecord.setVisibility(View.VISIBLE);
						etFilter.setVisibility(View.GONE);
					} else {
						adapter = new BuildAdapter<String>(BuildActivity.this,
								R.layout.build_item, R.id.tvListItem, list);
						imgNoRecord.setVisibility(View.GONE);
						etFilter.setVisibility(View.VISIBLE);
					}
					lvFiles.setAdapter(adapter);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				list = MobileAPI.getFileList(folder, 60);
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	@Override
	public void onItemClick(AdapterView<?> adapter, View v, int position,
			long id) {

		MobileAPI.downloadFile(this, folder,
				(String) lvFiles.getItemAtPosition(position));

	}

}
