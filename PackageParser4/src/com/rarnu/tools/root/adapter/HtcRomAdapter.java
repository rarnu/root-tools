package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.common.HtcRomInfo;
import com.rarnu.tools.root.holder.HtcRomAdapterHolder;

public class HtcRomAdapter extends InnerAdapter<HtcRomInfo> {

	private boolean checkable;
	
	public HtcRomAdapter(Context context, List<HtcRomInfo> list) {
		super(context, list);
	}
	
	public void setCheckable(boolean checkable) {
		this.checkable = checkable;
		notifyDataSetChanged();
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.htcrom_item, parent, false);
		}
		HtcRomAdapterHolder holder = (HtcRomAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new HtcRomAdapterHolder();
			holder.itemIcon = (ImageView) v.findViewById(R.id.itemIcon);
			holder.itemName = (TextView) v.findViewById(R.id.itemName);
			holder.itemDesc = (TextView) v.findViewById(R.id.itemDesc);
			holder.chkSelect = (CheckBox) v.findViewById(R.id.chkSelect);
			v.setTag(holder);
		}
		final HtcRomInfo item = list.get(position);
		if (item != null) {
			if (item.icon == 0) {
				holder.itemIcon.setImageBitmap(null);
			} else {
				holder.itemIcon.setImageResource(item.icon);
			}
			holder.itemName.setText(item.title);
			holder.itemDesc.setText(item.desc);
			holder.chkSelect.setChecked(item.checked);
			holder.chkSelect.setEnabled(checkable);
			
			holder.chkSelect.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View v) {
					item.checked = ((CheckBox) v).isChecked();
				}
			});
		}
		return v;
	}

	@Override
	public String getValueText(HtcRomInfo item) {
		return "";
	}

}
