package com.rarnu.tools.root.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.HtcRomAdapter;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.HtcRomInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.receiver.MutaxReceiver;
import com.rarnu.tools.root.receiver.MutaxReceiver.OnReceiveMessage;
import com.rarnu.tools.root.service.HtcRomService;

public class HtcRomFragment extends BaseFragment implements OnReceiveMessage {

	ListView lstRomCleaner;
	DataProgressBar progressHtcRom;

	List<HtcRomInfo> list = null;
	HtcRomAdapter adapter = null;

	MutaxReceiver receiver;

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
		receiver.register(getActivity());
	}

	@Override
	public void onPause() {
		receiver.unregister(getActivity());
		super.onPause();
	}

	@Override
	protected void initComponents() {
		lstRomCleaner = (ListView) innerView.findViewById(R.id.lstRomCleaner);
		progressHtcRom = (DataProgressBar) innerView
				.findViewById(R.id.progressHtcRom);

		list = new ArrayList<HtcRomInfo>();
		adapter = new HtcRomAdapter(getActivity(), list);
		lstRomCleaner.setAdapter(adapter);

		receiver = new MutaxReceiver(Actions.ACTION_CLEANING_HTC, null, null);
		receiver.setOnReceiveMessage(this);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_htcrom;
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
		if (getSelectedItemCount() == 0) {
			Toast.makeText(getActivity(), R.string.no_clean_item_selected,
					Toast.LENGTH_LONG).show();
			return;
		}

		new AlertDialog.Builder(getActivity())
				.setTitle(R.string.clean_htc_rom)
				.setMessage(R.string.clean_htc_rom_confirm)
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								doCleanRom();

							}
						}).setNegativeButton(R.string.cancel, null).show();

	}

	private void doCleanRom() {
		setCleaningState(true);
		// clean service
		Intent inHtcRomService = new Intent(getActivity(), HtcRomService.class);
		inHtcRomService.putExtra("command", buildCommand());
		inHtcRomService.putExtra("id", RTConsts.NOTIFY_ID_HTC_ROM);
		inHtcRomService.putExtra("title", R.string.clean_htc_rom);
		inHtcRomService.putExtra("desc", R.string.clean_htc_rom_finish);
		inHtcRomService.putExtra("proc_id", RTConsts.NOTIFY_PROC_HTC_ROM);
		inHtcRomService.putExtra("proc_title", R.string.clean_htc_rom);
		inHtcRomService.putExtra("proc_desc", R.string.cleaning_proc);
		getActivity().startService(inHtcRomService);
	}

	private String buildCommand() {
		String cmd = "";

		for (int i = 0; i < list.size(); i++) {
			cmd += list.get(i).checked ? "1" : "0";
		}
		return cmd;
	}

	private void setAllItemUnchecked() {
		for (int i = 0; i < list.size(); i++) {
			list.get(i).checked = false;
		}
		adapter.setNewList(list);
	}

	private int getSelectedItemCount() {
		int ret = 0;
		for (int i = 0; i < list.size(); i++) {
			if (list.get(i).checked) {
				ret++;
			}
		}
		return ret;
	}

	private void setCleaningState(boolean cleaning) {
		try {
			progressHtcRom.setAppName(getString(R.string.cleaning_htcrom));
			progressHtcRom.setVisibility(cleaning ? View.VISIBLE : View.GONE);

			adapter.setCheckable(!cleaning);
			if (!cleaning) {
				setAllItemUnchecked();
			}
		} catch (Exception e) {

		}
	}

	@Override
	protected void initLogic() {

		list.clear();
		
		HtcRomInfo infoCustom = new HtcRomInfo();
		infoCustom.title = getString(R.string.itm_custom);
		infoCustom.desc = getString(R.string.itmdesc_custom);
		infoCustom.icon = 0;
		list.add(infoCustom);

		HtcRomInfo infoCar = new HtcRomInfo();
		infoCar.title = getString(R.string.itm_car);
		infoCar.desc = getString(R.string.itmdesc_car);
		infoCar.icon = 0;
		list.add(infoCar);

		HtcRomInfo infoFacebook = new HtcRomInfo();
		infoFacebook.title = getString(R.string.itm_facebook);
		infoFacebook.desc = getString(R.string.itmdesc_facebook);
		infoFacebook.icon = 0;
		list.add(infoFacebook);

		HtcRomInfo infoTwitter = new HtcRomInfo();
		infoTwitter.title = getString(R.string.itm_twitter);
		infoTwitter.desc = getString(R.string.itmdesc_twitter);
		infoTwitter.icon = 0;
		list.add(infoTwitter);

		HtcRomInfo infoDropbox = new HtcRomInfo();
		infoDropbox.title = getString(R.string.itm_dropbox);
		infoDropbox.desc = getString(R.string.itmdesc_dropbox);
		infoDropbox.icon = 0;
		list.add(infoDropbox);

		HtcRomInfo infoSkydrive = new HtcRomInfo();
		infoSkydrive.title = getString(R.string.itm_skydrive);
		infoSkydrive.desc = getString(R.string.itmdesc_skydrive);
		infoSkydrive.icon = 0;
		list.add(infoSkydrive);

		HtcRomInfo infoLaputa = new HtcRomInfo();
		infoLaputa.title = getString(R.string.itm_laputa);
		infoLaputa.desc = getString(R.string.itmdesc_laputa);
		infoLaputa.icon = 0;
		list.add(infoLaputa);

		HtcRomInfo infoFlickr = new HtcRomInfo();
		infoFlickr.title = getString(R.string.itm_flickr);
		infoFlickr.desc = getString(R.string.itmdesc_flickr);
		infoFlickr.icon = 0;
		list.add(infoFlickr);

		HtcRomInfo infoFriendstream = new HtcRomInfo();
		infoFriendstream.title = getString(R.string.itm_friendstream);
		infoFriendstream.desc = getString(R.string.itmdesc_friendstream);
		infoFriendstream.icon = 0;
		list.add(infoFriendstream);

		HtcRomInfo infoGoogle = new HtcRomInfo();
		infoGoogle.title = getString(R.string.itm_google);
		infoGoogle.desc = getString(R.string.itmdesc_google);
		infoGoogle.icon = 0;
		list.add(infoGoogle);

		HtcRomInfo info3rd = new HtcRomInfo();
		info3rd.title = getString(R.string.itm_3rd);
		info3rd.desc = getString(R.string.itmdesc_3rd);
		info3rd.icon = 0;
		list.add(info3rd);
		
		HtcRomInfo infoCm3rd = new HtcRomInfo();
		infoCm3rd.title = getString(R.string.itm_cm3rd);
		infoCm3rd.desc = getString(R.string.itmdesc_cm3rd);
		infoCm3rd.icon = 0;
		list.add(infoCm3rd);

		adapter.setNewList(list);
		adapter.setCheckable(true);
	}

	@Override
	public void onStateChange(boolean operating) {
		if (!operating) {
			Intent inHtcRomService = new Intent(getActivity(),
					HtcRomService.class);
			getActivity().stopService(inHtcRomService);
		}
		setCleaningState(operating);

	}

	@Override
	public void onProgress(String name, int position, int total) {

	}

	@Override
	public void onMutaxMessage(boolean operating) {

	}

}
