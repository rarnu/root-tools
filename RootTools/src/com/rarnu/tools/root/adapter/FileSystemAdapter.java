package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.FileSystemFileInfo;
import com.rarnu.tools.root.holder.FileSystemHolder;

import java.util.List;

public class FileSystemAdapter extends BaseAdapter<FileSystemFileInfo> {
    public FileSystemAdapter(Context context, List<FileSystemFileInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(FileSystemFileInfo item) {
        return item.name;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.filesystem_item, parent, false);
        }
        FileSystemHolder holder = (FileSystemHolder) v.getTag();
        if (holder == null) {
            holder = new FileSystemHolder();
            holder.imgIcon = (ImageView) v.findViewById(R.id.imgIcon);
            holder.tvFilename = (TextView) v.findViewById(R.id.tvFilename);
            v.setTag(holder);
        }
        FileSystemFileInfo item = list.get(position);
        if (item != null) {
            holder.tvFilename.setText(item.name);
            holder.imgIcon.setBackgroundResource(item.isDirectory ? R.drawable.format_folder : item.icon);
        }
        return v;
    }
}
