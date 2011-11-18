package com.snda.roottools;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.Toast;
import android.widget.AdapterView.OnItemClickListener;

public class GetToolActivity extends Activity implements OnItemClickListener {

	public static final String BASE_URL = "http://rarnu.7thgen.info/snda/roottool/";
	public static final String DOWNLOAD_DIR = "/sdcard/download/";

	public static final int MSG_DOWNLOAD_START = 10;
	public static final int MSG_DOWNLOAD_PROGRESS = 11;
	public static final int MSG_DOWNLOAD_FINISH = 12;
	public static final int MSG_DOWNLOAD_ERROR = 19;

	ListView lvTools;
	ProgressBar pbLoadTool;
	ProgressBar pbDownloading;

	List<ToolInfo> list_tool = null;
	ToolAdapter adapter = null;
	String test = "";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.get_tool);

		File fDownload = new File(DOWNLOAD_DIR);
		if (!fDownload.exists()) {
			fDownload.mkdirs();
		}

		lvTools = (ListView) findViewById(R.id.lvTools);
		pbLoadTool = (ProgressBar) findViewById(R.id.pbLoadTool);
		pbDownloading = (ProgressBar) findViewById(R.id.pbDownloading);
		lvTools.setOnItemClickListener(this);
		getToolsT();
	}

	private void getToolsT() {
		pbLoadTool.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (list_tool == null) {
						Toast.makeText(GetToolActivity.this,
								R.string.get_no_tool, Toast.LENGTH_LONG).show();
					}
					lvTools.setAdapter(adapter);
					pbLoadTool.setVisibility(View.GONE);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				list_tool = getToolInfo();
				if (list_tool != null) {
					adapter = new ToolAdapter(getLayoutInflater(), list_tool);
				}
				h.sendEmptyMessage(1);
			}
		}).start();

	}

	private List<ToolInfo> getToolInfo() {
		List<ToolInfo> result = null;
		try {
			String ret = Network
					.CallGet(BASE_URL + "tools.php", "", HTTP.UTF_8);
			test = ret;
			JSONObject json = new JSONObject(ret);
			JSONArray arr = json.getJSONArray("data");
			result = new ArrayList<ToolInfo>();
			for (int i = 0; i < arr.length(); i++) {
				ToolInfo info = new ToolInfo();
				info.name = arr.getJSONObject(i).getString("name");
				info.version = arr.getJSONObject(i).getString("version");
				info.version_code = arr.getJSONObject(i).getInt("version_code");
				info.size = arr.getJSONObject(i).getString("size");
				info.desc = arr.getJSONObject(i).getString("desc");
				info.apk = arr.getJSONObject(i).getString("apk");
				info.packageName = arr.getJSONObject(i).getString(
						"package_name");
				result.add(info);
			}

		} catch (Exception e) {
			Log.e("error", e.getMessage());

		}
		return result;
	}

	private void installApkT(final String filePath) {
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					MainActivity.listInstalled = MainActivity
							.getInstalledRootTools(GetToolActivity.this);
					lvTools.setAdapter(null);
					lvTools.setAdapter(adapter);
					pbDownloading.setProgress(0);
					pbDownloading.setVisibility(View.GONE);
					lvTools.setEnabled(true);
					Toast.makeText(GetToolActivity.this,
							R.string.download_install_ok, Toast.LENGTH_LONG)
							.show();
				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				RootUtils.runRootCommand("pm install -r " + filePath);
				h.sendEmptyMessage(1);
			}
		}).start();
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			setResult(RESULT_OK);
		}
		return super.onKeyDown(keyCode, event);
	}

	private void downloadT(final String url, final String filePath) {
		pbDownloading.setVisibility(View.VISIBLE);
		pbDownloading.setProgress(0);
		lvTools.setEnabled(false);
		Toast.makeText(this, R.string.start_download, Toast.LENGTH_LONG).show();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == MSG_DOWNLOAD_START) {
					pbDownloading.setMax(msg.arg2);
					pbDownloading.setProgress(msg.arg1);
				} else if (msg.what == MSG_DOWNLOAD_PROGRESS) {
					pbDownloading.setProgress(msg.arg1);
				} else if (msg.what == MSG_DOWNLOAD_FINISH) {
					installApkT(filePath);
				} else if (msg.what == MSG_DOWNLOAD_ERROR) {
					pbDownloading.setProgress(0);
					pbDownloading.setVisibility(View.GONE);
					lvTools.setEnabled(true);
					Toast.makeText(GetToolActivity.this,
							R.string.download_error, Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				downloadFile(url, filePath, h);
			}
		}).start();

	}

	public static void downloadFile(String url, String filePath, Handler h) {
		URL url2 = null;
		int filesize = 0;
		int position = 0;
		try {
			url2 = new URL(url);
			HttpURLConnection con = (HttpURLConnection) url2.openConnection();
			InputStream in = con.getInputStream();
			filesize = con.getContentLength();
			if (h != null) {
				Message msg = new Message();
				msg.what = MSG_DOWNLOAD_START;
				msg.arg1 = position;
				msg.arg2 = filesize;
				h.sendMessage(msg);
			}
			File fileOut = new File(filePath);
			FileOutputStream out = new FileOutputStream(fileOut);
			byte[] bytes = new byte[1024];
			int c;
			while ((c = in.read(bytes)) != -1) {
				out.write(bytes, 0, c);
				position += c;

				if (h != null) {
					Message msg = new Message();
					msg.what = MSG_DOWNLOAD_PROGRESS;
					msg.arg1 = position;
					msg.arg2 = filesize;
					h.sendMessage(msg);
				}
			}
			in.close();
			out.close();
			if (h != null) {
				Message msg = new Message();
				msg.what = MSG_DOWNLOAD_FINISH;
				msg.arg1 = 0;
				msg.arg2 = filesize;
				h.sendMessage(msg);
			}
		} catch (Exception e) {
			h.sendEmptyMessage(MSG_DOWNLOAD_ERROR);
		}
	}

	@Override
	public void onItemClick(AdapterView<?> adapter, View view, int position,
			long id) {
		ToolInfo info = (ToolInfo) lvTools.getItemAtPosition(position);
		if (MainActivity.isToolInstalled(info.packageName, info.version_code) == 1) {
			return;
		}

		final String url = BASE_URL + info.apk;
		final String apk = DOWNLOAD_DIR + info.apk;

		new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(
				String.format(getResources().getString(
						R.string.download_msg_fmt), info.name, info.size))
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface arg0, int arg1) {
								downloadT(url, apk);

							}
						}).setNegativeButton(R.string.cancel, null).show();
	}
}
