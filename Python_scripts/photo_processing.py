import os
from PIL import Image
from pillow_heif import register_heif_opener

# Enables the library to open HEIC files
register_heif_opener()

def batch_process_real_estate(input_folder, output_folder, max_mb=2.9):
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    max_bytes = max_mb * 1024 * 1024

    for filename in os.listdir(input_folder):
        if filename.lower().endswith(('.jpg', '.jpeg', '.heic', '.png')):
            file_path = os.path.join(input_folder, filename)
            
            with Image.open(file_path) as img:
                # Ensure the image is in RGB (strips transparency/alpha)
                if img.mode != 'RGB':
                    img = img.convert('RGB')
                
                # Setup output path
                target_name = os.path.splitext(filename)[0] + ".jpg"
                save_path = os.path.join(output_folder, target_name)
                
                # Start with high quality and decrease until it fits the 3MB limit
                quality = 95
                img.save(save_path, "JPEG", quality=quality, optimize=True)
                
                while os.path.getsize(save_path) > max_bytes and quality > 30:
                    quality -= 5
                    img.save(save_path, "JPEG", quality=quality, optimize=True)
                
                print(f"Done: {target_name} | Size: {os.path.getsize(save_path)/1024/1024:.2f} MB")

# Use your actual paths here
batch_process_real_estate('C:/Users/JerónimoRodríguezEsc/OneDrive - World Wildlife Fund, Inc/personal/Photos-3-001', 'C:/casa_vecino')