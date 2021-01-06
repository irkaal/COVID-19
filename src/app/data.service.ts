import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root',
})
export class DataService {
  constructor(private _httpClient: HttpClient) {}

  public getData(): Promise<{
    [Country_Region: string]: { [Field: string]: string[] };
  }> {
    return new Promise((resolve, reject) => {
      this.downloadData().subscribe(
        (data: string) => {
          resolve(this.parseData(data));
        },
        (error: any) => {
          reject(error);
        }
      );
    });
  }

  private downloadData(): Observable<string> {
    console.log('Downloading data...');

    return this._httpClient.get('assets/data/data.csv', {
      responseType: 'text',
    });
  }

  private parseData(
    data: string
  ): { [Country_Region: string]: { [Field: string]: string[] } } {
    console.log('Parsing data...');

    const rows: string[] = data.split('\n');

    const out: any = {};
    const headers: string[] = rows[0].split(',');
    for (let i = 1; i < rows.length; i++) {
      const rowValues = rows[i].split(',');
      if (rowValues.length < headers.length) {
        continue;
      }

      const countryRegion: string =
        rowValues[12] + rowValues[13] + rowValues[14];

      rowValues.forEach((value: string, index: number) => {
        if (!out[countryRegion]) {
          out[countryRegion] = {};
        }
        if (headers[index] in out[countryRegion]) {
          out[countryRegion][headers[index]].push(value);
        } else {
          out[countryRegion][headers[index]] = [value];
        }
      });
    }

    return out;
  }
}
