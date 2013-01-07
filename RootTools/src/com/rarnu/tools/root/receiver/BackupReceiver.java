package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import com.rarnu.tools.root.common.Actions;

public class BackupReceiver extends BroadcastReceiver {

	public IntentFilter filter = null;
	public IntentFilter progressFilter = null;
	public IntentFilter mutaxFilter = null;

	private OnBackupState onBackup;
	
	public BackupReceiver(String main, String progress, String mutax) {
		filter = new IntentFilter(main);
		progressFilter = new IntentFilter(progress);
		mutaxFilter = new IntentFilter(mutax);
	}

	public interface OnBackupState {
		void onBackupStateChange(boolean operating);

		void onBackupProgress(String name, int position, int total);

		void onMutaxMessage(boolean operating);
	}

	public void setOnBackupState(OnBackupState onBackup) {
		this.onBackup = onBackup;
	}

	public void register(Context context) {
		context.registerReceiver(this, filter);
		context.registerReceiver(this, progressFilter);
		context.registerReceiver(this, mutaxFilter);
	}

	public void unregister(Context context) {
		context.unregisterReceiver(this);
	}

	@Override
	public void onReceive(Context context, Intent intent) {
		if (intent != null) {
			String action = intent.getAction();
			if (action != null) {

				if (action.equals(Actions.ACTION_BACKUP)) {
					boolean operating = intent.getBooleanExtra("operating",
							false);
					if (onBackup != null) {
						onBackup.onBackupStateChange(operating);
					}
				} else if (action.equals(Actions.ACTION_BACKUP_PROGRESS)) {
					int size = intent.getIntExtra("size", 0);
					int position = intent.getIntExtra("position", 0);
					String name = intent.getStringExtra("name");
					if (onBackup != null) {
						onBackup.onBackupProgress(name, position, size);
					}
				} else if (action.equals(Actions.ACTION_RESTORE)) {
					boolean operating = intent.getBooleanExtra("operating",
							false);
					if (onBackup != null) {
						onBackup.onMutaxMessage(operating);
					}

				}

			}
		}

	}

}
