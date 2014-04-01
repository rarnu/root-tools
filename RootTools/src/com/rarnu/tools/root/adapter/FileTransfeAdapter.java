package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FileTransferItem;
import com.rarnu.tools.root.holder.FileTransferAdapterHolder;

import java.util.List;

public class FileTransfeAdapter extends BaseAdapter<FileTransferItem> {

    public FileTransfeAdapter(Context context, List<FileTransferItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(FileTransferItem item) {
        return null;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.file_transfer_item, parent, false);
        }
        FileTransferAdapterHolder holder = (FileTransferAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new FileTransferAdapterHolder();
            holder.tvFileName = (TextView) v.findViewById(R.id.tvFileName);
            holder.pbTransfer = (ProgressBar) v.findViewById(R.id.pbTransfer);
            v.setTag(holder);
        }
        FileTransferItem item = list.get(position);
        if (item != null) {
            holder.tvFileName.setText(item.fileName);
            holder.pbTransfer.setMax(100);
            holder.pbTransfer.setProgress((int) (item.progress * 1.0D * 100 / item.total));
            holder.pbTransfer.setVisibility(item.inProgress ? View.VISIBLE : View.GONE);
        }
        return v;
    }
}
