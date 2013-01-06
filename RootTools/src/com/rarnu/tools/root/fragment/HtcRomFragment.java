package com.rarnu.tools.root.fragment;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.HtcRomItem;
import com.rarnu.tools.root.service.HtcRomService;

public class HtcRomFragment extends BaseFragment {

	HtcRomItem itmCar, itmFacebook, itmTwitter, itmDropbox, itmSkydrive,
			itmLaputa, itmFlickr, itmFriendStream, itmGoogle, itm3rd;
	DataProgressBar progressHtcRom;

	@Override
	protected int getBarTitle() {
		return R.string.clean_htc_rom;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.clean_htc_rom_with_path;
	}
	
	@Override
	public void onResume() {
		super.onResume();
		getActivity().registerReceiver(receiver, filter);
	}
	
	@Override
	public void onPause() {
		getActivity().unregisterReceiver(receiver);
		super.onPause();
	}

	@Override
	protected void initComponents() {
		progressHtcRom = (DataProgressBar) innerView
				.findViewById(R.id.progressHtcRom);
		itmCar = (HtcRomItem) innerView.findViewById(R.id.itmCar);
		itmFacebook = (HtcRomItem) innerView.findViewById(R.id.itmFacebook);
		itmTwitter = (HtcRomItem) innerView.findViewById(R.id.itmTwitter);
		itmDropbox = (HtcRomItem) innerView.findViewById(R.id.itmDropbox);
		itmSkydrive = (HtcRomItem) innerView.findViewById(R.id.itmSkydrive);
		itmLaputa = (HtcRomItem) innerView.findViewById(R.id.itmLaputa);
		itmFlickr = (HtcRomItem) innerView.findViewById(R.id.itmFlickr);
		itmFriendStream = (HtcRomItem) innerView
				.findViewById(R.id.itmFriendStream);
		itmGoogle = (HtcRomItem) innerView.findViewById(R.id.itmGoogle);
		itm3rd = (HtcRomItem) innerView.findViewById(R.id.itm3rd);

		itmCar.setName(R.string.itm_car);
		itmCar.setDesc(R.string.itmdesc_car);
		itmFacebook.setName(R.string.itm_facebook);
		itmFacebook.setDesc(R.string.itmdesc_facebook);
		itmTwitter.setName(R.string.itm_twitter);
		itmTwitter.setDesc(R.string.itmdesc_twitter);
		itmDropbox.setName(R.string.itm_dropbox);
		itmDropbox.setDesc(R.string.itmdesc_dropbox);
		itmSkydrive.setName(R.string.itm_skydrive);
		itmSkydrive.setDesc(R.string.itmdesc_skydrive);
		itmLaputa.setName(R.string.itm_laputa);
		itmLaputa.setDesc(R.string.itmdesc_laputa);
		itmFlickr.setName(R.string.itm_flickr);
		itmFlickr.setDesc(R.string.itmdesc_flickr);
		itmFriendStream.setName(R.string.itm_friendstream);
		itmFriendStream.setDesc(R.string.itmdesc_friendstream);
		itmGoogle.setName(R.string.itm_google);
		itmGoogle.setDesc(R.string.itmdesc_google);
		itm3rd.setName(R.string.itm_3rd);
		itm3rd.setDesc(R.string.itmdesc_3rd);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_more_htcrom;
	}

	@Override
	protected void initMenu(Menu menu) {
		MenuItem itemClean = menu.add(0, MenuItemIds.MENU_CLEAN, 99,
				R.string.clean);
		itemClean.setIcon(android.R.drawable.ic_menu_delete);
		itemClean.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_CLEAN:
			cleanHtcRom();
			break;
		}
		return true;
	}

	private void cleanHtcRom() {
		if (!itmCar.isChecked() && !itmFacebook.isChecked()
				&& !itmTwitter.isChecked() && !itmDropbox.isChecked()
				&& !itmSkydrive.isChecked() && !itmLaputa.isChecked()
				&& !itmFlickr.isChecked() && !itmFriendStream.isChecked()
				&& !itmGoogle.isChecked() && !itm3rd.isChecked()) {
			Toast.makeText(getActivity(), R.string.no_clean_item_selected,
					Toast.LENGTH_LONG).show();
			return;
		}

		AlertDialogEx.showAlertDialogEx(getActivity(),
				getString(R.string.clean_htc_rom),
				getString(R.string.clean_htc_rom_confirm),
				getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						doCleanRom();

					}
				}, getString(R.string.cancel), null);
	}

	private void doCleanRom() {
		setCleaningState(true);
		// clean service
		Intent inHtcRomService = new Intent(getActivity(), HtcRomService.class);
		inHtcRomService.putExtra("command", buildCommand());
		inHtcRomService.putExtra("id", RTConsts.NOTIFY_ID_HTC_ROM);
		inHtcRomService.putExtra("title", R.string.clean_htc_rom);
		inHtcRomService.putExtra("desc", R.string.clean_htc_rom_finish);
		getActivity().startService(inHtcRomService);
	}

	private String buildCommand() {
		String cmd = "";
		cmd += itmCar.isChecked() ? "1" : "0";
		cmd += itmFacebook.isChecked() ? "1" : "0";
		cmd += itmTwitter.isChecked() ? "1" : "0";
		cmd += itmDropbox.isChecked() ? "1" : "0";
		cmd += itmSkydrive.isChecked() ? "1" : "0";
		cmd += itmLaputa.isChecked() ? "1" : "0";
		cmd += itmFlickr.isChecked() ? "1" : "0";
		cmd += itmFriendStream.isChecked() ? "1" : "0";
		cmd += itmGoogle.isChecked() ? "1" : "0";
		cmd += itm3rd.isChecked() ? "1" : "0";
		return cmd;
	}

	private void setCleaningState(boolean cleaning) {
		try {
			progressHtcRom.setAppName(getString(R.string.cleaning_htcrom));
			progressHtcRom.setVisibility(cleaning ? View.VISIBLE : View.GONE);

			if (cleaning) {
				itmCar.disable();
				itmFacebook.disable();
				itmTwitter.disable();
				itmDropbox.disable();
				itmSkydrive.disable();
				itmLaputa.disable();
				itmFlickr.disable();
				itmFriendStream.disable();
				itmGoogle.disable();
				itm3rd.disable();
			} else {
				itmCar.enable();
				itmFacebook.enable();
				itmTwitter.enable();
				itmDropbox.enable();
				itmSkydrive.enable();
				itmLaputa.enable();
				itmFlickr.enable();
				itmFriendStream.enable();
				itmGoogle.enable();
				itm3rd.enable();

				itmCar.setChecked(false);
				itmFacebook.setChecked(false);
				itmTwitter.setChecked(false);
				itmDropbox.setChecked(false);
				itmSkydrive.setChecked(false);
				itmLaputa.setChecked(false);
				itmFlickr.setChecked(false);
				itmFriendStream.setChecked(false);
				itmGoogle.setChecked(false);
				itm3rd.setChecked(false);
			}
		} catch (Exception e) {

		}
	}

	@Override
	protected void initLogic() {

	}

	public class CleanHtcReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			boolean operating = intent.getBooleanExtra("operating", false);
			if (!operating) {
				Intent inHtcRomService = new Intent(getActivity(), HtcRomService.class);
				getActivity().stopService(inHtcRomService);
			}
			setCleaningState(operating);
			
		}
	}

	public CleanHtcReceiver receiver = new CleanHtcReceiver();
	public IntentFilter filter = new IntentFilter(Actions.ACTION_CLEANING_HTC);

}
