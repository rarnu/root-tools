package com.rarnu.nfc;

import java.util.List;
import java.util.Locale;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.net.Uri;
import android.nfc.NdefMessage;
import android.nfc.NdefRecord;
import android.nfc.NfcAdapter;
import android.nfc.Tag;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.nfc.record.NdefMessageParser;
import com.rarnu.nfc.record.NfcBindConfig;
import com.rarnu.nfc.record.NfcUtils;
import com.rarnu.nfc.record.NfcUtils.OnNfcResolved;
import com.rarnu.nfc.record.ParsedNdefRecord;

public class MainActivity extends Activity implements OnNfcResolved,
		OnClickListener {

	private TextView tvRead;
	private Button btnWrite;
	private EditText etWrite;
	private EditText etBindTag, etBindComponent;
	private Button btnSaveOrReplace;

	private NfcAdapter mAdapter;
	private PendingIntent mPendingIntent;

	// 0: read
	// 1: write
	private int mode = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		tvRead = (TextView) findViewById(R.id.tvRead);
		btnWrite = (Button) findViewById(R.id.btnWrite);
		etWrite = (EditText) findViewById(R.id.etWrite);
		etBindTag = (EditText) findViewById(R.id.etBindTag);
		etBindComponent = (EditText) findViewById(R.id.etBindComponent);
		btnSaveOrReplace = (Button) findViewById(R.id.btnSaveOrReplace);

		btnWrite.setOnClickListener(this);
		btnSaveOrReplace.setOnClickListener(this);

		NfcUtils.resolveIntent(getIntent(), this);
		mAdapter = NfcAdapter.getDefaultAdapter(this);
		mPendingIntent = PendingIntent.getActivity(this, 0, new Intent(this,
				getClass()).addFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP), 0);

	}

	@Override
	protected void onResume() {
		super.onResume();
		if (mAdapter != null) {
			mAdapter.enableForegroundDispatch(this, mPendingIntent, null, null);
		}
	}

	@Override
	protected void onPause() {
		super.onPause();
		if (mAdapter != null) {
			mAdapter.disableForegroundDispatch(this);

		}
	}

	void buildTagViews(NdefMessage[] msgs) {
		if (msgs == null || msgs.length == 0) {
			return;
		}

		List<ParsedNdefRecord> records = NdefMessageParser.parse(msgs[0]);
		final int size = records.size();
		if (size != 0) {
			ParsedNdefRecord record = records.get(0);
			tvRead.setText(record.getNfcText());
			etBindTag.setText(record.getNfcText());

			String component = NfcBindConfig.loadBindConfig(this,
					record.getNfcText());
			dispatchComponent(component);
		}

	}

	@Override
	public void onNewIntent(Intent intent) {
		setIntent(intent);
		if (mode == 0) {
			NfcUtils.resolveIntent(intent, this);
		} else {
			String msg = etWrite.getText().toString();
			if (msg == null || msg.equals("")) {
				Toast.makeText(this, "cannot write empty message.",
						Toast.LENGTH_SHORT).show();
				return;
			}
			Tag detectedTag = intent.getParcelableExtra(NfcAdapter.EXTRA_TAG);

			NdefMessage ndefMsg = new NdefMessage(
					new NdefRecord[] { NfcUtils.newTextRecord(msg,
							Locale.ENGLISH, true) });
			try {
				boolean ret = NfcUtils.writeTag(ndefMsg, detectedTag);
				Toast.makeText(this,
						ret ? "Write Tag finished." : "Write Tag failed.",
						Toast.LENGTH_SHORT).show();
				onClick(btnWrite);
			} catch (Exception e) {
				Toast.makeText(this, e.getMessage(), Toast.LENGTH_SHORT).show();
			}
		}
	}

	@Override
	public void resolvedNfcMessage(NdefMessage[] msgs) {
		buildTagViews(msgs);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnWrite:
			if (mode == 0) {
				mode = 1;
				btnWrite.setText("Write Mode Now");
			} else {
				mode = 0;
				btnWrite.setText("Read Mode Now");
			}
			break;
		case R.id.btnSaveOrReplace:
			String component = etBindComponent.getText().toString();
			String tag = etBindTag.getText().toString();
			if ((tag != null) && (!tag.equals(""))) {
				NfcBindConfig.saveBindConfig(this, tag, component);
				Toast.makeText(this, "saved.", Toast.LENGTH_SHORT).show();
			}
			break;
		}

	}

	private void gotoApp(Context context, String namespace, String activity) {
		Log.e(getClass().getName(), String.format("ns:%s, act:%s", namespace, activity));
		if (applicationInstalled(namespace)) {
			startApplication(namespace, activity);
		} else {
			openGooglePlayForApp(context, namespace);
		}
	}

	private boolean applicationInstalled(String namespace) {
		try {
			PackageInfo info = getPackageManager().getPackageInfo(namespace, 0);
			return info != null;
		} catch (NameNotFoundException e) {
			return false;
		}
	}

	private boolean startApplication(String namespace, String activity) {
		try {
			String cmd = "am start -a android.intent.action.MAIN -c android.intent.category.LAUNCHER -n %s/%s";
			Runtime.getRuntime().exec(String.format(cmd, namespace, activity));
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	private void openGooglePlayForApp(Context context, String namespace) {
		Intent inPlay = new Intent(Intent.ACTION_VIEW);
		inPlay.setData(Uri.parse("market://details?id=" + namespace));
		context.startActivity(inPlay);
	}

	private void dispatchComponent(String component) {
		if (component == null || component.equals("")) {
			return;
		}
		if (!component.contains("/")) {
			return;
		}
		String ns = component.substring(0, component.indexOf("/"));
		String activity = component.substring(component.indexOf("/") + 1);
		
		gotoApp(this, ns, activity);
	}
}
