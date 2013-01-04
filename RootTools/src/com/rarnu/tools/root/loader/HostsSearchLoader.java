package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.dns.NSLookup;
import com.rarnu.tools.root.dns.record.Address;
import com.rarnu.tools.root.utils.DIPairUtils;

public class HostsSearchLoader extends AsyncTaskLoader<List<HostRecordInfo>> {

	private String domain;
	
	public HostsSearchLoader(Context context) {
		super(context);
	}

	@Override
	public List<HostRecordInfo> loadInBackground() {
		List<Address> listAddress = NSLookup.nslookup(domain,
				GlobalInstance.nameServer);
		return DIPairUtils.toPairList(domain, listAddress);
	}

	public void setDomain(String domain) {
		this.domain = domain;
	}
	
	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<HostRecordInfo> data) {
		super.onCanceled(data);
	}

	@Override
	protected void onStopLoading() {
		cancelLoad();
	}

	@Override
	protected void onReset() {
		stopLoading();

	}
}
