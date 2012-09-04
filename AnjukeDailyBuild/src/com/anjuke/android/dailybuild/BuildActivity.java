package com.anjuke.android.dailybuild;

import java.util.List;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.anjuke.android.dailybuild.api.MobileAPI;

@SuppressLint("HandlerLeak")
public class BuildActivity extends Activity implements OnItemClickListener,
		OnClickListener, OnItemLongClickListener {

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
		lvFiles.setOnItemLongClickListener(this);
		tvLoading.setOnClickListener(this);

		etFilter.addTextChangedListener(new TextWatcher() {

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
		// imgNoRecord.setVisibility(View.GONE);
		tvLoading.setText(R.string.loading);
		tvLoading.setEnabled(false);
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tvLoading.setText(R.string.reload);
					tvLoading.setEnabled(true);
					if (list == null) {
						adapter = null;
						imgNoRecord.setVisibility(View.VISIBLE);

					} else {
						adapter = new BuildAdapter<String>(BuildActivity.this,
								R.layout.build_item, R.id.tvListItem, list);
						imgNoRecord.setVisibility(View.GONE);

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

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvLoading:
			getFileListT(folder);
			break;
		}

	}

	@Override
	public boolean onItemLongClick(AdapterView<?> adapter, View v,
			final int position, long id) {
		String fileName = (String) lvFiles.getItemAtPosition(position);
		new AlertDialog.Builder(this)
				.setTitle(fileName)
				.setItems(
						new String[] { getString(R.string.download),
								getString(R.string.comment) },
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								switch (which) {
								case 0:
									MobileAPI.downloadFile(
											BuildActivity.this,
											folder,
											(String) lvFiles
													.getItemAtPosition(position));
									break;
								case 1:
									sendMail((String) lvFiles
											.getItemAtPosition(position));
									break;
								}

							}
						}).setNegativeButton(R.string.cancel, null).show();
		return false;
	}

	private void sendMail(String fileName) {
		Intent intent = new Intent(Intent.ACTION_SENDTO, Uri.parse("mailto:dl-mobile@anjuke.com"));
		// intent.setType("message/rfc822");

		intent.putExtra(Intent.EXTRA_SUBJECT,
				String.format(getString(R.string.feedback), folder, fileName));

		intent.putExtra(Intent.EXTRA_TEXT, String.format(
				getString(R.string.feedback_content), folder, fileName));

//		intent.putExtra(Intent.EXTRA_EMAIL,
//				new String[] { "dl-mobile@anjuke.com" });

		startActivity(Intent
				.createChooser(intent, getString(R.string.sendmail)));
	}

}
