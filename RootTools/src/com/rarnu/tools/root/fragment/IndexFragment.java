package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceClickListener;
import android.view.Menu;

import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BasePreferenceFragment;
import com.rarnu.devlib.common.FragmentStarter;
import com.rarnu.devlib.common.UIInstance;
import com.rarnu.devlib.component.PreferenceEx;
import com.rarnu.tools.root.Fragments;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.fragmentactivity.AboutActivity;
import com.rarnu.tools.root.fragmentactivity.BusyboxActivity;
import com.rarnu.tools.root.fragmentactivity.CleanCacheMainActivity;
import com.rarnu.tools.root.fragmentactivity.CompMainActivity;
import com.rarnu.tools.root.fragmentactivity.DataBackupActivity;
import com.rarnu.tools.root.fragmentactivity.DataRestoreActivity;
import com.rarnu.tools.root.fragmentactivity.DiskInfoActivity;
import com.rarnu.tools.root.fragmentactivity.EnableappMainActivity;
import com.rarnu.tools.root.fragmentactivity.FileSystemActivity;
import com.rarnu.tools.root.fragmentactivity.HardUpdateActivity;
import com.rarnu.tools.root.fragmentactivity.HostMainActivity;
import com.rarnu.tools.root.fragmentactivity.HtcRomActivity;
import com.rarnu.tools.root.fragmentactivity.MemMainActivity;
import com.rarnu.tools.root.fragmentactivity.RecommandActivity;
import com.rarnu.tools.root.fragmentactivity.SettingsActivity;
import com.rarnu.tools.root.fragmentactivity.SysappMainActivity;
import com.rarnu.tools.root.fragmentactivity.TerminalActivity;
import com.rarnu.tools.root.fragmentactivity.UserFeedbackActivity;
import com.rarnu.tools.root.utils.BusyboxUtils;
import com.rarnu.tools.root.utils.DalvikUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.utils.MiscUtils;
import com.rarnu.utils.NetworkUtils;

public class IndexFragment extends BasePreferenceFragment implements
		OnPreferenceClickListener {

	PreferenceEx prefSysApp, prefSysAppEnabled, prefComponent, prefRoot,
			prefHtcRom, prefHardUpdate;
	PreferenceEx prefBackup, prefRestore;
	PreferenceEx prefCleanMemory, prefCleanCache, prefCleanDalvik,
			prefDiskInfo, prefFileSystem;
	PreferenceEx prefHosts, prefScanMedia, prefNetworkState, prefReboot;
	PreferenceEx prefFeedback, prefRecommand, prefAbout;
	PreferenceEx prefTerminal;
	PreferenceEx prefSettings;

	public void showFunctionalEnabledTags() {
		boolean isRooted = RootUtils.hasSu();
		boolean isWrongRooted = RootUtils.isWrongRoot();
		if (isWrongRooted) {
			isRooted = false;
		}
		prefSysApp.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefSysAppEnabled.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefHardUpdate.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefComponent.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefRoot.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefHtcRom.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefBackup.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefRestore.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefCleanMemory.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefCleanCache.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefCleanDalvik.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefHosts.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefReboot.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefFileSystem.setStatus(isRooted ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_BANNED);
		prefScanMedia.setStatus(PreferenceEx.STATE_NORMAL);
		prefNetworkState.setStatus(PreferenceEx.STATE_NORMAL);
		prefFeedback.setStatus(PreferenceEx.STATE_NORMAL);
		prefRecommand.setStatus(PreferenceEx.STATE_NORMAL);
		prefAbout.setStatus(PreferenceEx.STATE_NORMAL);
		prefTerminal.setStatus(PreferenceEx.STATE_NORMAL);

		if (isRooted) {
			showBusyboxTag();
		}
	}

	private void showBusyboxTag() {
		boolean ready = BusyboxUtils.isSuBusyboxReady();
		prefSysApp.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefSysAppEnabled.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefHardUpdate.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefRoot.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefBackup.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefRestore.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefCleanCache.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefHosts.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefReboot.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
		prefFileSystem.setStatus(ready ? PreferenceEx.STATE_NORMAL
				: PreferenceEx.STATE_WARNING);
	}

	@Override
	public boolean onPreferenceClick(Preference preference) {
		// system
		if (preference.getKey().equals(getString(R.string.id_sysapp))) {
			UIInstance.currentFragment = 1;
			FragmentStarter.showContent(getActivity(),
					SysappMainActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_SYSAPP));
		} else if (preference.getKey().equals(
				getString(R.string.id_sysappenabled))) {
			UIInstance.currentFragment = 2;
			FragmentStarter.showContent(getActivity(),
					EnableappMainActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_ENABLEAPP));

		} else if (preference.getKey()
				.equals(getString(R.string.id_hardupdate))) {
			UIInstance.currentFragment = 16;
			FragmentStarter.showContent(getActivity(),
					HardUpdateActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_HARD_UPDATE));
		} else if (preference.getKey().equals(getString(R.string.id_component))) {
			UIInstance.currentFragment = 3;
			FragmentStarter.showContent(getActivity(), CompMainActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_COMP));

		} else if (preference.getKey().equals(getString(R.string.id_root))) {
			UIInstance.currentFragment = 4;
			FragmentStarter.showContent(getActivity(), BusyboxActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_BUSYBOX));

		} else if (preference.getKey().equals(getString(R.string.id_cleanhtc))) {
			UIInstance.currentFragment = 5;
			FragmentStarter.showContent(getActivity(), HtcRomActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_HTCROM));

		}
		// backup
		else if (preference.getKey().equals(getString(R.string.id_backup))) {
			UIInstance.currentFragment = 6;
			FragmentStarter.showContent(getActivity(),
					DataBackupActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_BACKUP));
		} else if (preference.getKey().equals(getString(R.string.id_restore))) {
			UIInstance.currentFragment = 14;
			FragmentStarter.showContent(getActivity(),
					DataRestoreActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_RESTORE));
		}

		// memory
		else if (preference.getKey().equals(getString(R.string.id_cleanmemory))) {
			UIInstance.currentFragment = 7;
			FragmentStarter.showContent(getActivity(), MemMainActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_MEM));
		} else if (preference.getKey()
				.equals(getString(R.string.id_cleancache))) {
			UIInstance.currentFragment = 8;
			FragmentStarter.showContent(getActivity(),
					CleanCacheMainActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_CLEAN_CACHE));
		} else if (preference.getKey().equals(
				getString(R.string.id_cleandalvik))) {
			DalvikUtils.doCleanDalvikT(getActivity(), getView(),
					prefCleanDalvik);
		} else if (preference.getKey().equals(getString(R.string.id_diskinfo))) {
			UIInstance.currentFragment = 17;
			FragmentStarter.showContent(getActivity(), DiskInfoActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_DISKINFO));
		} else if (preference.getKey()
				.equals(getString(R.string.id_filesystem))) {
			UIInstance.currentFragment = 18;
			FragmentStarter.showContent(getActivity(),
					FileSystemActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_FILESYSTEM));
		}

		// other
		else if (preference.getKey().equals(getString(R.string.id_hosts))) {
			UIInstance.currentFragment = 9;
			FragmentStarter.showContent(getActivity(), HostMainActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_HOST));
		} else if (preference.getKey().equals(getString(R.string.id_scanmedia))) {
			MiscUtils.doScanMedia(getActivity());
		} else if (preference.getKey().equals(getString(R.string.id_network))) {
			new AlertDialog.Builder(getActivity())
					.setTitle(R.string.check_network_status)
					.setMessage(
							NetworkUtils.getNetworkStatusDesc(getActivity()))
					.setPositiveButton(R.string.ok, null).show();
		} else if (preference.getKey().equals(getString(R.string.id_reboot))) {
			new AlertDialog.Builder(getActivity())
					.setTitle(R.string.reboot_device)
					.setMessage(R.string.confirm_reboot)
					.setPositiveButton(R.string.ok,
							new DialogInterface.OnClickListener() {

								@Override
								public void onClick(DialogInterface dialog,
										int which) {
									DeviceUtils.reboot();

								}
							}).setNegativeButton(R.string.cancel, null).show();
		}

		// support
		else if (preference.getKey().equals(getString(R.string.id_feedback))) {
			UIInstance.currentFragment = 10;
			FragmentStarter.showContent(getActivity(),
					UserFeedbackActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_FEEDBACK));
		} else if (preference.getKey().equals(getString(R.string.id_recommand))) {
			UIInstance.currentFragment = 11;
			FragmentStarter.showContent(getActivity(), RecommandActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_RECOMMAND));
		} else if (preference.getKey().equals(getString(R.string.id_about))) {
			UIInstance.currentFragment = 12;
			FragmentStarter.showContent(getActivity(), AboutActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_ABOUT));
		}
		// emu
		else if (preference.getKey()
				.equals(getString(R.string.id_terminal_emu))) {
			UIInstance.currentFragment = 15;
			FragmentStarter.showContent(getActivity(), TerminalActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_TERMINAL));
		}

		//
		else if (preference.getKey().equals(getString(R.string.id_settings))) {
			UIInstance.currentFragment = 13;
			FragmentStarter.showContent(getActivity(), SettingsActivity.class,
					Fragments.getFragment(FragmentNameConst.FN_SETTINGS));
		}

		return true;
	}

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	public void initComponents() {
		prefSysApp = (PreferenceEx) findPreference(getString(R.string.id_sysapp));
		prefSysAppEnabled = (PreferenceEx) findPreference(getString(R.string.id_sysappenabled));
		prefHardUpdate = (PreferenceEx) findPreference(getString(R.string.id_hardupdate));
		prefComponent = (PreferenceEx) findPreference(getString(R.string.id_component));
		prefRoot = (PreferenceEx) findPreference(getString(R.string.id_root));
		prefHtcRom = (PreferenceEx) findPreference(getString(R.string.id_cleanhtc));
		prefBackup = (PreferenceEx) findPreference(getString(R.string.id_backup));
		prefRestore = (PreferenceEx) findPreference(getString(R.string.id_restore));
		prefCleanMemory = (PreferenceEx) findPreference(getString(R.string.id_cleanmemory));
		prefCleanCache = (PreferenceEx) findPreference(getString(R.string.id_cleancache));
		prefCleanDalvik = (PreferenceEx) findPreference(getString(R.string.id_cleandalvik));
		prefDiskInfo = (PreferenceEx) findPreference(getString(R.string.id_diskinfo));
		prefFileSystem = (PreferenceEx) findPreference(getString(R.string.id_filesystem));
		prefHosts = (PreferenceEx) findPreference(getString(R.string.id_hosts));
		prefScanMedia = (PreferenceEx) findPreference(getString(R.string.id_scanmedia));
		prefNetworkState = (PreferenceEx) findPreference(getString(R.string.id_network));
		prefReboot = (PreferenceEx) findPreference(getString(R.string.id_reboot));
		prefFeedback = (PreferenceEx) findPreference(getString(R.string.id_feedback));
		prefRecommand = (PreferenceEx) findPreference(getString(R.string.id_recommand));
		prefAbout = (PreferenceEx) findPreference(getString(R.string.id_about));
		prefSettings = (PreferenceEx) findPreference(getString(R.string.id_settings));
		prefTerminal = (PreferenceEx) findPreference(getString(R.string.id_terminal_emu));
	}

	@Override
	public void initEvents() {
		prefSysApp.setOnPreferenceClickListener(this);
		prefSysAppEnabled.setOnPreferenceClickListener(this);
		prefHardUpdate.setOnPreferenceClickListener(this);
		prefComponent.setOnPreferenceClickListener(this);
		prefRoot.setOnPreferenceClickListener(this);
		prefHtcRom.setOnPreferenceClickListener(this);
		prefBackup.setOnPreferenceClickListener(this);
		prefRestore.setOnPreferenceClickListener(this);
		prefCleanMemory.setOnPreferenceClickListener(this);
		prefCleanCache.setOnPreferenceClickListener(this);
		prefCleanDalvik.setOnPreferenceClickListener(this);
		prefDiskInfo.setOnPreferenceClickListener(this);
		prefFileSystem.setOnPreferenceClickListener(this);
		prefHosts.setOnPreferenceClickListener(this);
		prefScanMedia.setOnPreferenceClickListener(this);
		prefNetworkState.setOnPreferenceClickListener(this);
		prefReboot.setOnPreferenceClickListener(this);
		prefFeedback.setOnPreferenceClickListener(this);
		prefRecommand.setOnPreferenceClickListener(this);
		prefAbout.setOnPreferenceClickListener(this);
		prefSettings.setOnPreferenceClickListener(this);
		prefTerminal.setOnPreferenceClickListener(this);
	}

	@Override
	public void initLogic() {
		showFunctionalEnabledTags();

	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.xml.main;
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
