#!/usr/bin/env sh

if ! command -v xclip &> /dev/null; then
    echo "Please install xclip"
    exit 1
fi

# Function to process files and directories
process_file_or_directory() {
    local path="$1"
    if [[ -d "$path" ]]; then
        for file in "$path"/*; do
            process_file_or_directory "$file"
        done
    elif [[ -f "$path" ]]; then
        # Check if the file is a text file
        if file "$path" | grep -qE 'text|ASCII'; then
            local file_name=$(basename "$path")
            echo "<$file_name>"
            cat "$path"
            echo "</$file_name>"
            echo
        fi
    fi
}

result=""
for file in "$@"; do
    if [[ -e "$file" ]]; then
        content=$(process_file_or_directory "$file")
        result="$result$content"
    else
        echo "File or directory $file does not exist."
        exit 1
    fi
done

if command -v xclip &> /dev/null; then
    echo -e "$result" | xclip -selection clipboard
elif command -v pbcopy &> /dev/null; then
    echo -e "$result" | pbcopy
fi

echo "Selected files' content wrapped and copied to clipboard."
